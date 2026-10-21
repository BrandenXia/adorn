{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Executor (executeTasks, ExecutorCfg (..)) where

import Control.Applicative ((<|>))
import Control.Exception (IOException, try)
import Control.Monad (guard, join, unless, void)
import Control.Monad.Extra (allM, whenMaybe)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe, runMaybeT)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Maybe (isJust)
import qualified Data.Text.IO as TIO
import GHC.IO.Exception (ExitCode (..))
import Path
import System.Directory (copyFile, createDirectoryIfMissing)
import System.Process.Extra (readProcessWithExitCode)
import Text.HTML.TagSoup (parseTags, renderTags)

import Action (Action (..))
import Html (appendStyle, extractTag)
import Layout
import Task (Task (..))

data ExecutorCfg = ExecutorCfg
  { buildDir :: Path Abs Dir
  , layoutDir :: Path Abs Dir
  , layouts :: HashSet (Path Rel Dir)
  }

data ExecutorState = ExecutorState
  { layoutCache :: HashMap (Path Rel Dir) (Maybe Layout)
  }

type Executor = ReaderT ExecutorCfg (StateT ExecutorState IO)

executeTasks :: [Task] -> ExecutorCfg -> IO Bool
executeTasks tasks cfg = do
  let initialState = ExecutorState{layoutCache = HM.empty}
  evalStateT (runReaderT (allM executeTask tasks) cfg) initialState

executeTask :: (MonadReader ExecutorCfg m, MonadState ExecutorState m, MonadIO m) => Task -> m Bool
executeTask = fmap isJust . runMaybeT . execute
 where
  execute Task{..} = do
    unless (action == Ignore) $ tryIO $ ensureParentExists target
    case action of
      Ignore -> pure ()
      Copy -> tryIO $ copyFile (toFilePath source) (toFilePath target)
      TypstPDF -> compile source target ["-f", "pdf"]
      Typst{..} -> do
        compileTarget <- asks $ (</> relative) . buildDir
        tryIO $ ensureParentExists compileTarget
        compile source compileTarget ["-f", "html", "--features", "html"]

        template <- liftIO $ traverse (readLayout . fromSomeFile) template

        layoutDir <- asks layoutDir
        layouts <- asks layouts
        parentLayout <- do
          cache <- gets layoutCache
          (result, newCache) <-
            lift $ runStateT (getLayout layoutDir layouts (parent relative)) cache
          modify' $ \s -> s{layoutCache = newCache}
          pure result

        layout <- hoistMaybe $ parentLayout <> template

        tmpHtml <- tryIO $ TIO.readFile $ toFilePath compileTarget
        let tags = parseTags tmpHtml
            css = extractTag "style" tags
            body = extractTag "body" tags
        let applied = applyLayout layout $ \case
              "child" -> body
              _ -> ""
        let appended = appendStyle (parseTags applied) css
        tryIO $ TIO.writeFile (toFilePath target) (renderTags appended)

  tryIO :: (MonadIO m) => IO a -> MaybeT m a
  tryIO action = MaybeT $ do
    result <- liftIO $ try @IOException action
    pure $ either (const Nothing) Just result

  ensureParentExists path = createDirectoryIfMissing True $ toFilePath $ parent path

  compile source target flags = do
    ok <- tryIO $ typstCompile source target flags
    guard ok

  typstCompile source target flags =
    readProcessWithExitCode "typst" (["compile"] ++ flags ++ [toFilePath source, toFilePath target]) ""
      >>= \case
        (ExitSuccess, _, _) -> pure True
        (ExitFailure code, stdout, stderr) -> do
          putStrLn $ "Typst failed with exit code " ++ show code
          putStrLn $ "Stdout: " ++ stdout
          putStrLn $ "Stderr: " ++ stderr
          pure False
