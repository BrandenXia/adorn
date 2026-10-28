{-# LANGUAGE RecordWildCards #-}

module Task (Task (..), DiscoveryCfg (..), discoverTask, taskDepsExist, taskNecessary) where

import Control.Monad (filterM)
import Control.Monad.Extra (notM)
import Data.Array (Array, bounds, inRange, (!))
import Data.Char (isDigit)
import Data.List.Extra (firstJust)
import Data.Maybe (mapMaybe)
import Path
import System.Directory (getModificationTime)
import System.Directory.Extra (doesFileExist)
import Text.Read (readMaybe)
import Text.Regex.TDFA

import Action
import Layout (LayoutSet, getFileLayout, toLayoutFile)
import Mapping (Mapping (..))

data Task = Task
  { source :: Path Abs File
  , target :: Path Abs File
  , relative :: Path Rel File
  , action :: Action Resolved
  , taskDeps :: [Path Abs File]
  }
  deriving (Show, Eq)

data DiscoveryCfg = DiscoveryCfg
  { workingDir :: Path Abs Dir
  , sourceDir :: Path Abs Dir
  , outputDir :: Path Abs Dir
  , layoutRoot :: Path Abs Dir
  , layoutSet :: LayoutSet
  , mappings :: [Mapping]
  }

discoverTask :: DiscoveryCfg -> [Path Abs File] -> [Task]
discoverTask cfg = mapMaybe (discoverFileTask cfg)

discoverFileTask :: DiscoveryCfg -> Path Abs File -> Maybe Task
discoverFileTask cfg@DiscoveryCfg{..} abs = do
  rel <- stripProperPrefix sourceDir abs
  firstJust (applyMapping cfg abs rel) mappings

applyMapping :: DiscoveryCfg -> Path Abs File -> Path Rel File -> Mapping -> Maybe Task
applyMapping DiscoveryCfg{..} abs rel Mapping{..} =
  case matchOnceText srcPat (toFilePath rel) of
    Nothing -> Nothing
    Just (_, groups, _) -> do
      targetRel <- parseRelFile (substitute groups dstTpl)
      let resolvedAction = resolveAction workingDir action
      Just $
        Task abs (outputDir </> targetRel) targetRel resolvedAction $
          abs
            : actionDeps resolvedAction
            ++ map ((layoutRoot </>) . toLayoutFile) (getFileLayout layoutSet rel)

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

taskDepsExist :: Task -> IO [Path Abs File]
taskDepsExist Task{..} = filterM (notM . doesFileExist . toFilePath) taskDeps

taskNecessary :: Task -> IO Bool
taskNecessary Task{action = Ignore} = return False
taskNecessary Task{..} = do
  mtimes <- mapM (getModificationTime . toFilePath) taskDeps
  targetExists <- doesFileExist (toFilePath target)
  if not targetExists
    then return True
    else do
      targetMtime <- getModificationTime (toFilePath target)
      return $ any (> targetMtime) mtimes
