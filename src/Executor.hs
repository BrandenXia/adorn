{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Executor (executeTasks, ExecutorCfg (..)) where

import Control.Applicative ((<|>))
import Control.Exception (IOException, displayException, try)
import Control.Monad (guard, join, unless, void)
import Control.Monad.Extra (allM, whenMaybe)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe, runMaybeT)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List (partition)
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.IO.Exception (ExitCode (..))
import Path
import System.Directory (copyFile, createDirectoryIfMissing)
import System.IO (hPutStrLn, stderr)
import System.Process.Extra (readProcessWithExitCode)
import Text.HTML.TagSoup (innerText, parseTags, renderTags)

import Action (Action (..))
import Html (appendStyle, extractTag, setTitle)
import Index (indexContext)
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
      (indexes, regular) = partition isIndex tasks
      isIndex Task{action = Index{}} = True
      isIndex _ = False
  evalStateT (runReaderT (allM executeTask (regular ++ indexes)) cfg) initialState

executeTask :: (MonadReader ExecutorCfg m, MonadState ExecutorState m, MonadIO m) => Task -> m Bool
executeTask = fmap isJust . runMaybeT . execute
 where
  execute Task{..} = do
    unless (action == Ignore) $ tryIO $ ensureParentExists target
    case action of
      Ignore -> pure ()
      Copy -> tryIO $ copyFile (toFilePath source) (toFilePath target)
      TypstPDF -> compile source target ["-f", "pdf"]
      Index{..} -> do
        template <- tryIO $ readLayout $ toFilePath indexTemplate
        parentLayout <- loadParentLayout relative
        context <- tryIO $ indexContext relative taskEntries
        let layout = maybe template (<> template) parentLayout
        tryIO $ TIO.writeFile (toFilePath target) (applyLayout layout context)
      Typst{..} -> do
        compileTarget <- asks $ (</> relative) . buildDir
        tryIO $ ensureParentExists compileTarget
        compile source compileTarget ["-f", "html", "--features", "html"]

        template <- tryIO $ traverse (readLayout . toFilePath) template

        parentLayout <- loadParentLayout relative

        layout <- hoistMaybe $ parentLayout <> template

        tmpHtml <- tryIO $ TIO.readFile $ toFilePath compileTarget
        let tags = parseTags tmpHtml
            css = extractTag "style" tags
            body = extractTag "body" tags
            title = innerText $ parseTags $ extractTag "title" tags
        let context = HM.fromList $ ("child", body) : [("title", title) | not (T.null title)]
            applied = applyLayout layout context
        let appended = appendStyle (parseTags applied) css
            titled = if T.null title then appended else setTitle appended title
        tryIO $ TIO.writeFile (toFilePath target) (renderTags titled)

  loadParentLayout relative = do
    layoutDir <- asks layoutDir
    layouts <- asks layouts
    cache <- gets layoutCache
    (result, newCache) <-
      tryIO $ runStateT (getLayout layoutDir layouts (parent relative)) cache
    modify' $ \s -> s{layoutCache = newCache}
    pure result

  tryIO :: (MonadIO m) => IO a -> MaybeT m a
  tryIO action = MaybeT $ do
    result <- liftIO $ try @IOException action
    case result of
      Left err -> do
        liftIO $ hPutStrLn stderr (displayException err)
        pure Nothing
      Right value -> pure (Just value)

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
