{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Executor (executePlan) where

import Control.Monad (forM, when)
import Control.Monad.Except (ExceptT)
import Control.Monad.State.Strict (StateT, evalStateT, get, modify')
import Control.Monad.Trans.Class (lift)
import qualified Data.Aeson as A
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.IO as TIO
import Path
import Text.Mustache (toMustache)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, removeFile)

import BuildError
import Cleanup (finishChoices)
import Html (documentBody, parseDocument)
import Index (indexContext)
import Layout (Layout, applyLayout, readLayout)
import Plan (BuildPlan, pendingTasks, planTasks, planBuildDir, planOutputDir, planMappingFile, planChoices, planProtected)
import Task
import Typst (compileTypst, readDependencies)

type Executor = StateT (HashMap (Path Abs File) Layout) (ExceptT BuildError IO)

executePlan :: BuildPlan -> ExceptT BuildError IO ()
executePlan plan = do
  evalStateT (mapM_ (executeTask $ planBuildDir plan) $ pendingTasks plan) HM.empty
  imported <- if null (planChoices plan) then pure [] else
    concat <$> forM (filter taskNeedsTypst $ planTasks plan) (\task -> do
      paths <- readDependencies $ taskCacheFile (planBuildDir plan) task "dependencies"
      forM paths $ \path -> buildIO "resolve Typst input" (Just path) $ parseAbsFile path)
  finishChoices (planBuildDir plan) (planOutputDir plan) (planMappingFile plan)
    (planChoices plan) (imported ++ planProtected plan)

executeTask :: Path Abs Dir -> Task -> Executor ()
executeTask buildDir task@Task{..} = forTarget (toFilePath target) $ do
  let cache = taskCacheFile buildDir task
      state = cache "state"
      deps = cache "dependencies"
      html = cache "compiled.html"
  buildIO "create output directory" (Just $ toFilePath $ parent target) $
    createDirectoryIfMissing True $ toFilePath $ parent target
  buildIO "create build state directory" (Just $ taskCacheDir buildDir task) $
    createDirectoryIfMissing True $ taskCacheDir buildDir task
  buildIO "invalidate build state" (Just state) $ do
    exists <- doesFileExist state
    when exists $ removeFile state
  case operation of
    CopyFile source -> copy source
    ComposeFile source ->
      if null taskLayouts then copy source else do
        content <- buildIO "read composition source" (Just $ toFilePath source) $ TIO.readFile $ toFilePath source
        compose $ HM.singleton "child" $ toMustache content
    CompilePdf source -> lift $ compileTypst source (toFilePath target) ["-f", "pdf"] deps
    CompileHtml source bodyOnly -> do
      lift $ compileTypst source html ["-f", "html", "--features", "html"] deps
      content <- buildIO "read compiled HTML" (Just html) $ TIO.readFile html
      let document = parseDocument content
      if bodyOnly then write $ documentBody document
        else compose $ HM.singleton "child" $ toMustache content
    GenerateIndex entries -> do
      context <- buildIO "read index entries" Nothing $ indexContext relative entries
      compose context
  buildIO "save build state" (Just state) $ TIO.writeFile state (taskSignature task)
  buildIO "report completed action" Nothing $ putStrLn $
    actionName ++ maybe "" ((" " ++) . toFilePath) (taskSource task) ++ " => " ++ toFilePath target
 where
  actionName = case operation of
    CopyFile{} -> "copy"
    ComposeFile{} -> "compose"
    CompileHtml{} -> "typst"
    CompilePdf{} -> "typstPDF"
    GenerateIndex{} -> "index"
  compose context = do
    layout <- loadLayouts taskLayouts
    write $ applyLayout layout $ HM.insert "meta" (toMustache $ A.Object taskContext) context
  copy source = buildIO "copy source" (Just $ toFilePath source) $ copyFile (toFilePath source) (toFilePath target)
  write = buildIO "write output" (Just $ toFilePath target) . TIO.writeFile (toFilePath target)

loadLayouts :: [Path Abs File] -> Executor (Maybe Layout)
loadLayouts [] = pure Nothing
loadLayouts paths = Just . mconcat <$> mapM load paths
 where
  load path = do
    cache <- get
    case HM.lookup path cache of
      Just layout -> pure layout
      Nothing -> do
        layout <- buildIO "compile layout" (Just $ toFilePath path) $ readLayout $ toFilePath path
        modify' $ HM.insert path layout
        pure layout
