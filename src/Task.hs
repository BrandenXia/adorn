{-# LANGUAGE RecordWildCards #-}

module Task (Task (..), discoverTask, taskDepsExist, taskNecessary) where

import Control.Monad (filterM)
import Control.Monad.Extra (notM)
import Data.Array (Array, bounds, inRange, (!))
import Data.Char (isDigit)
import Data.List.Extra (firstJust)
import Data.Maybe (mapMaybe)
import Path (Abs, Dir, File, Path, Rel, SomeBase, fromSomeFile, parseRelFile, stripProperPrefix, toFilePath, (</>))
import System.Directory (getModificationTime)
import System.Directory.Extra (doesFileExist)
import Text.Read (readMaybe)
import Text.Regex.TDFA

import Action (Action, deps)
import Mapping (Mapping (..))

data Task = Task
  { source :: Path Abs File
  , target :: Path Abs File
  , relative :: Path Rel File
  , action :: Action
  }
  deriving (Show, Eq)

discoverTask :: Path Abs Dir -> Path Abs Dir -> [Mapping] -> [Path Abs File] -> [Task]
discoverTask = ((mapMaybe .) .) . discoverFileTask

discoverFileTask :: Path Abs Dir -> Path Abs Dir -> [Mapping] -> Path Abs File -> Maybe Task
discoverFileTask base out mappings abs = do
  rel <- stripProperPrefix base abs
  firstJust
    (applyMapping out abs rel)
    mappings

applyMapping :: Path Abs Dir -> Path Abs File -> Path Rel File -> Mapping -> Maybe Task
applyMapping out abs rel Mapping{..} =
  case matchOnceText srcPat (toFilePath rel) of
    Nothing -> Nothing
    Just (_, groups, _) -> do
      targetRel <- parseRelFile (substitute groups dstTpl)
      Just $ Task abs (out </> targetRel) targetRel action

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
taskDepsExist Task{..} = filterM (notM . doesFileExist . fromSomeFile) $ deps action

taskNecessary :: Task -> IO Bool
taskNecessary Task{..} = do
  mtimes <- mapM getModificationTime (toFilePath source : map fromSomeFile (deps action))
  targetExists <- doesFileExist (toFilePath target)
  if not targetExists
    then return True
    else do
      targetMtime <- getModificationTime (toFilePath target)
      return $ any (> targetMtime) mtimes
