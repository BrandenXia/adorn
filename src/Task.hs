{-# LANGUAGE RecordWildCards #-}

module Task (Task (..), IndexEntry (..), DiscoveryCfg (..), discoverTask, taskDepsExist, taskNecessary) where

import Control.Monad (filterM)
import Control.Monad.Extra (notM)
import Data.Array (Array, bounds, inRange, (!))
import Data.Char (isDigit)
import Data.List.Extra (firstJust)
import Data.List (sortOn)
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
  , taskEntries :: [IndexEntry]
  }
  deriving (Show, Eq)

data IndexEntry = IndexEntry
  { entrySource :: Path Rel File
  , entryTarget :: Path Rel File
  , entryOutput :: Path Abs File
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
discoverTask cfg@DiscoveryCfg{..} files = map attachEntries tasks
 where
  tasks = mapMaybe (discoverFileTask cfg) files ++ mapMaybe generatedTask mappings
  generatedTask GeneratedMapping{action = Index template selection, ..} = do
    targetRel <- parseRelFile dstTpl
    let resolved = resolveAction workingDir (Index template selection)
    pure $ makeTask cfg (indexTemplate resolved) targetRel resolved
  generatedTask _ = Nothing

  attachEntries task@Task{action = Index{..}} =
    task{taskEntries = selected selection}
  attachEntries task = task

  selected pattern =
    case makeRegexM ("\\`" ++ pattern ++ "\\'") of
      Nothing -> [] -- Selections are validated when parsing the mapping.
      Just regex -> sortOn entrySource $ mapMaybe (selectEntry regex) tasks

  selectEntry _ Task{action = Ignore} = Nothing
  selectEntry _ Task{action = Index{}} = Nothing
  selectEntry regex Task{..} = do
    rel <- stripProperPrefix sourceDir source
    if matchTest (regex :: Regex) (toFilePath rel)
      then Just $ IndexEntry rel relative target
      else Nothing

discoverFileTask :: DiscoveryCfg -> Path Abs File -> Maybe Task
discoverFileTask cfg@DiscoveryCfg{..} abs = do
  rel <- stripProperPrefix sourceDir abs
  firstJust (applyMapping cfg abs rel) mappings

applyMapping :: DiscoveryCfg -> Path Abs File -> Path Rel File -> Mapping -> Maybe Task
applyMapping _ _ _ GeneratedMapping{} = Nothing
applyMapping cfg@DiscoveryCfg{..} abs rel Mapping{..} =
  case matchOnceText srcPat (toFilePath rel) of
    Nothing -> Nothing
    Just (_, groups, _) -> do
      targetRel <- parseRelFile (substitute groups dstTpl)
      let resolvedAction = resolveAction workingDir action
      Just $ makeTask cfg abs targetRel resolvedAction

makeTask :: DiscoveryCfg -> Path Abs File -> Path Rel File -> Action Resolved -> Task
makeTask DiscoveryCfg{..} source targetRel action =
  Task source (outputDir </> targetRel) targetRel action deps []
 where
  deps = source : actionDeps action
    ++ map ((layoutRoot </>) . toLayoutFile) (getFileLayout layoutSet targetRel)

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
-- Index membership can change when a source is added, removed, or remapped.
taskNecessary Task{action = Index{}} = return True
taskNecessary Task{..} = do
  mtimes <- mapM (getModificationTime . toFilePath) taskDeps
  targetExists <- doesFileExist (toFilePath target)
  if not targetExists
    then return True
    else do
      targetMtime <- getModificationTime (toFilePath target)
      return $ any (> targetMtime) mtimes
