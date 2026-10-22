{-# LANGUAGE RecordWildCards #-}

module Task (Task (..), discoverTask, taskDepsExist, taskNecessary) where

import Control.Monad (filterM)
import Control.Monad.Extra (notM)
import Data.Array (Array, bounds, inRange, (!))
import Data.Char (isDigit)
import qualified Data.HashSet as HS
import Data.List.Extra (firstJust)
import Data.Maybe (mapMaybe)
import Path
import System.Directory (getModificationTime)
import System.Directory.Extra (doesFileExist)
import Text.Read (readMaybe)
import Text.Regex.TDFA

import Action (Action (..), actionDeps)
import Layout (LayoutSet, getFileLayout, toLayoutFile)
import Mapping (Mapping (..))

data Task = Task
  { source :: Path Abs File
  , target :: Path Abs File
  , relative :: Path Rel File
  , action :: Action
  , taskDeps :: [SomeBase File]
  }
  deriving (Show, Eq)

discoverTask :: Path Abs Dir -> Path Abs Dir -> Path Abs Dir -> LayoutSet -> [Mapping] -> [Path Abs File] -> [Task]
discoverTask base out layout layoutSet mappings = mapMaybe (discoverFileTask base out layout layoutSet mappings)

discoverFileTask :: Path Abs Dir -> Path Abs Dir -> Path Abs Dir -> LayoutSet -> [Mapping] -> Path Abs File -> Maybe Task
discoverFileTask base out layout layoutSet mappings abs = do
  rel <- stripProperPrefix base abs
  firstJust
    (applyMapping out layout layoutSet abs rel)
    mappings

applyMapping :: Path Abs Dir -> Path Abs Dir -> LayoutSet -> Path Abs File -> Path Rel File -> Mapping -> Maybe Task
applyMapping out layout layoutSet abs rel Mapping{..} =
  case matchOnceText srcPat (toFilePath rel) of
    Nothing -> Nothing
    Just (_, groups, _) -> do
      targetRel <- parseRelFile (substitute groups dstTpl)
      Just $
        Task abs (out </> targetRel) targetRel action $
          Abs abs
            : actionDeps action
            ++ map (Abs . (layout </>) . toLayoutFile) (getFileLayout layoutSet rel)

substitute :: Array Int (String, (Int, Int)) -> String -> String
substitute groups = go
 where
  go [] = []
  go ('$' : '$' : xs) = '$' : go xs
  go ('$' : xs) =
    let (digits, rest) = span isDigit xs
     in case readMaybe digits of
          Just n | inRange (bounds groups) n -> fst (groups ! n) ++ go rest
          _ -> '$' : go xs
  go (x : xs) = x : go xs

taskDepsExist :: Task -> IO [SomeBase File]
taskDepsExist Task{..} = filterM (notM . doesFileExist . fromSomeFile) taskDeps

taskNecessary :: Task -> IO Bool
taskNecessary Task{action = Ignore} = return False
taskNecessary Task{..} = do
  mtimes <- mapM (getModificationTime . fromSomeFile) taskDeps
  targetExists <- doesFileExist (toFilePath target)
  if not targetExists
    then return True
    else do
      targetMtime <- getModificationTime (toFilePath target)
      return $ any (> targetMtime) mtimes
