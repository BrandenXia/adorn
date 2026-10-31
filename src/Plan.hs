{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Plan (PlanCfg (..), BuildPlan, buildPlan, pendingTasks, planTasks, planBuildDir, planOutputDir, planMappingFile, planChoices, planProtected, requiresTypst) where

import Control.Monad (filterM, forM, forM_, unless, when)
import Control.Monad.Except (ExceptT, liftEither)
import Control.Monad.Extra (anyM)
import qualified Data.Aeson.KeyMap as KM
import Data.Array (Array, bounds, inRange, (!))
import Data.Char (isDigit)
import qualified Data.HashMap.Strict as HM
import Data.List (nub, partition, sort, sortOn)
import Data.Maybe (catMaybes, maybeToList)
import qualified Data.Text.IO as TIO
import Path
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, getModificationTime, listDirectory)
import qualified System.FilePath as FP
import Text.Read (readMaybe)
import Text.Regex.TDFA (matchOnceText)

import Action
import BuildError
import Layout (LayoutSet, getFileLayout, toLayoutFile)
import Mapping (Mapping (..))
import Script (runScript)
import Task
import Typst (readDependencies)

data PlanCfg = PlanCfg
  { workingDir :: Path Abs Dir
  , sourceDir :: Path Abs Dir
  , outputDir :: Path Abs Dir
  , buildDir :: Path Abs Dir
  , layoutRoot :: Path Abs Dir
  , mappingFile :: Path Abs File
  , layoutSet :: LayoutSet
  , mappings :: [Mapping]
  }

data BuildPlan = BuildPlan
  { pendingTasks :: [Task]
  , planTasks :: [Task]
  , planBuildDir :: Path Abs Dir
  , planOutputDir :: Path Abs Dir
  , planMappingFile :: Path Abs File
  , planChoices :: [(Path Abs File, Maybe (Path Abs File))]
  , planProtected :: [Path Abs File]
  }

requiresTypst :: BuildPlan -> Bool
requiresTypst = any taskNeedsTypst . pendingTasks

buildPlan :: PlanCfg -> Bool -> ExceptT BuildError IO BuildPlan
buildPlan cfg@PlanCfg{..} full = do
  when (sourceDir == outputDir || sourceDir == buildDir) $
    failBuild "configure source discovery" (Just $ toFilePath sourceDir)
      "The source directory must differ from the output and build directories."
  files <- discoverSources cfg
  (candidates, choices) <- discoverTasks cfg files
  validateTasks candidates
  tasks <- if full then pure candidates else filterM (taskNecessary buildDir) candidates
  let (indexes, regular) = partition taskIsIndex tasks
  pure $ BuildPlan (regular ++ indexes) candidates buildDir outputDir mappingFile choices
    (nub $ files ++ map target candidates ++ concatMap taskDeps candidates)

discoverSources :: PlanCfg -> ExceptT BuildError IO [Path Abs File]
discoverSources PlanCfg{..} = walk [] sourceDir
 where
  walk visited dir = do
    physical <- buildIO "resolve source directory" (Just $ toFilePath dir) $
      canonicalizePath (toFilePath dir) >>= parseAbsDir
    if physical == outputDir || physical == buildDir || physical `elem` visited
      then pure []
      else do
        names <- sort <$> buildIO "list source directory" (Just $ toFilePath dir) (listDirectory $ toFilePath dir)
        concat <$> forM names (visit (physical : visited) dir)
  visit visited dir name = do
    let path = toFilePath dir FP.</> name
    isDir <- buildIO "inspect source path" (Just path) $ doesDirectoryExist path
    if isDir
      then buildIO "resolve source directory" (Just path) (parseAbsDir path) >>= walk visited
      else do
        exists <- buildIO "inspect source file" (Just path) $ doesFileExist path
        if exists
          then (: []) <$> buildIO "resolve source file" (Just path) (parseAbsFile path)
          else pure []

discoverTasks :: PlanCfg -> [Path Abs File] -> ExceptT BuildError IO ([Task], [(Path Abs File, Maybe (Path Abs File))])
discoverTasks cfg@PlanCfg{..} files = do
  discovered <- mapM discoverFile files
  let fileTasks = catMaybes $ map fst discovered
      choices = [(source, fmap (target . fst) result)
                | (source, (result, True)) <- zip files discovered]
  indexes <- catMaybes <$> mapM discoverGenerated mappings
  let tasks = fileTasks ++ indexes
      entries = sortOn entrySource
        [ IndexEntry rel (relative task) (target task) (taskContext task)
        | (task, _) <- tasks
        , Just source <- [taskSource task]
        , Just rel <- [stripProperPrefix sourceDir source]
        ]
      attach (task, Nothing) = task
      attach (task, Just selection) = task{operation = GenerateIndex $
        filter (matchesSelection selection . toFilePath . entrySource) entries}
  pure (map attach tasks, choices)
 where
  discoverFile source = case stripProperPrefix sourceDir source of
    Nothing -> pure (Nothing, False)
    Just rel -> firstMatch source rel mappings
  firstMatch _ _ [] = pure (Nothing, False)
  firstMatch source rel (GeneratedMapping{} : rest) = firstMatch source rel rest
  firstMatch source rel (Mapping{..} : rest) =
    case matchOnceText srcPat (toFilePath rel) of
      Nothing -> firstMatch source rel rest
      Just (_, groups, _) -> do
        result <- select source groups mappingOrigin KM.empty []
          (resolveAction workingDir action) dstTpl
        pure (result, case action of Choose{} -> True; _ -> False)

  select source groups origin context scripts (Choose mode script branches) _ = do
    (name, nextContext) <- runScript mode script [] source context
    case [(act, dst) | (branch, act, dst) <- branches, branch == name] of
      [(act, dst)] -> select source groups origin nextContext (script : scripts) act dst
      _ -> failBuild "select branch" (Just origin) $
        "Script " ++ toFilePath script ++ " returned unknown branch " ++ show name
  select _ _ _ _ _ Ignore _ = pure Nothing
  select source groups origin context scripts act dst = do
    targetRel <- liftEither $ destination origin $ substitute groups dst
    pure $ fmap (\(task, selection) ->
      (task{taskContext = context, taskDeps = nub (scripts ++ taskDeps task)}, selection)) $
      fileTask cfg origin source targetRel act

  discoverGenerated GeneratedMapping{..} = do
    targetRel <- liftEither $ destination mappingOrigin dstTpl
    pure $ Just $ indexTask cfg mappingOrigin [] targetRel (resolveIndex workingDir indexSpec)
  discoverGenerated _ = pure Nothing

destination :: String -> String -> Either BuildError (Path Rel File)
destination origin raw = case parseRelFile raw of
  Just path -> Right path
  Nothing -> Left $ BuildError "resolve destination" (Just origin) Nothing $
    "Invalid output path " ++ show raw ++ "; expected a file path inside the output directory."

fileTask :: PlanCfg -> String -> Path Abs File -> Path Rel File -> Action Resolved -> Maybe (Task, Maybe Selection)
fileTask cfg origin source rel action = case action of
  Index spec -> Just $ indexTask cfg origin [source] rel spec
  Typst template bodyOnly ->
    let fragment = bodyOnly == Just True
     in file (CompileHtml source fragment) $
          if fragment then [] else parentLayouts cfg rel ++ maybeToList template
  TypstPDF -> file (CompilePdf source) []
  Copy -> file (CopyFile source) []
  Compose -> file (ComposeFile source) (parentLayouts cfg rel)
  Ignore -> Nothing
  Choose{} -> Nothing -- Choices are resolved before constructing tasks.
 where
  file op layouts = Just (makeTask cfg origin [source] rel op layouts, Nothing)

indexTask :: PlanCfg -> String -> [Path Abs File] -> Path Rel File -> IndexSpec Resolved -> (Task, Maybe Selection)
indexTask cfg origin inputs rel IndexSpec{..} =
  (makeTask cfg origin inputs rel (GenerateIndex []) (parentLayouts cfg rel ++ [indexTemplate]), Just selection)

parentLayouts :: PlanCfg -> Path Rel File -> [Path Abs File]
parentLayouts PlanCfg{..} rel = map ((layoutRoot </>) . toLayoutFile) $ getFileLayout layoutSet rel

makeTask :: PlanCfg -> String -> [Path Abs File] -> Path Rel File -> Operation -> [Path Abs File] -> Task
makeTask PlanCfg{..} origin inputs rel op layouts =
  Task (outputDir </> rel) rel op (nub $ mappingFile : inputs ++ layouts) layouts KM.empty origin

validateTasks :: [Task] -> ExceptT BuildError IO ()
validateTasks tasks = do
  let outputs = HM.fromListWith (++) [(toFilePath $ target task, [task]) | task <- tasks]
  forM_ (sortOn fst $ HM.toList outputs) $ \(path, writers) -> when (length writers > 1) $
    failBuild "validate outputs" (Just path) $
      "Multiple tasks write this output:\n" ++ unlines (map describe writers)
  forM_ tasks $ \task -> do
    let path = toFilePath $ target task
    forM_ (ancestors path) $ \ancestor -> when (HM.member ancestor outputs) $
      failBuild "validate outputs" (Just ancestor) $
        "An output file is also used as a directory by " ++ taskOrigin task
    forM_ (taskDeps task) $ \dependency -> do
      let input = toFilePath dependency
      when (HM.member input outputs) $
        failBuild "validate outputs" (Just input) "An output would overwrite a source, template, or configuration input."
      exists <- buildIO "check input" (Just input) $ doesFileExist input
      unless exists $ failBuild "validate input" (Just $ taskOrigin task) $ "Missing file: " ++ input
 where
  describe task = taskOrigin task ++ maybe " (generated index)" (\p -> " (" ++ toFilePath p ++ ")") (taskSource task)
  ancestors path =
    let parentDir = FP.takeDirectory path
     in if parentDir == path then [] else parentDir : ancestors parentDir

taskNecessary :: Path Abs Dir -> Task -> ExceptT BuildError IO Bool
taskNecessary buildDir task = anyM id
  [ pure $ taskIsIndex task
  , not <$> fileExists output
  , not <$> fileExists state
  , (/= taskSignature task) <$> buildIO "read build state" (Just state) (TIO.readFile state)
  , if taskNeedsTypst task then not <$> fileExists deps else pure False
  , do
      imported <- if taskNeedsTypst task then readDependencies deps else pure []
      outputTime <- buildIO "read output timestamp" (Just output) $ getModificationTime output
      anyM (changed outputTime) $ nub $ map toFilePath (taskDeps task) ++ imported
  ]
 where
  output = toFilePath $ target task
  state = taskCacheFile buildDir task "state"
  deps = taskCacheFile buildDir task "dependencies"
  fileExists path = buildIO "check build file" (Just path) $ doesFileExist path
  changed outputTime path = do
    exists <- fileExists path
    if not exists then pure True else do
      inputTime <- buildIO "read input timestamp" (Just path) $ getModificationTime path
      pure $ inputTime > outputTime

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
