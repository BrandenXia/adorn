{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Executor () where

import Control.Monad.Extra (maybeM, whenMaybe)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Data.Text.Template (Template)
import qualified Data.Text.Template as Tpl

import Action (Action (..))
import Control.Applicative ((<|>))
import Control.Monad (guard, join, (>=>))
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe, runMaybeT)
import System.FilePath (takeDirectory, (</>))
import Task (Task (..))

type AbsPath = FilePath
type RelPath = FilePath

newtype Layout = Layout Template deriving (Show, Eq)

readLayout :: AbsPath -> IO Layout
readLayout = TIO.readFile >=> (return . Layout . Tpl.template)

toLayout :: Text -> Layout
toLayout = Layout . Tpl.template

applyLayout :: Layout -> (Text -> Text) -> Text
applyLayout (Layout tpl) = TL.toStrict . Tpl.render tpl

composeLayouts :: Layout -> Layout -> Layout
composeLayouts outer (Layout innerTpl) = toLayout $ applyLayout outer ctx
 where
  ctx "child" = Tpl.showTemplate innerTpl
  ctx _ = ""

instance Semigroup Layout where
  (<>) = composeLayouts

data ExecutorCfg = ExecutorCfg
  { buildDir :: AbsPath
  , layoutDir :: AbsPath
  , layouts :: HashSet RelPath
  }

data ExecutorState = ExecutorState
  { layoutCache :: HashMap RelPath (Maybe Layout)
  }

type Executor = ReaderT ExecutorCfg (StateT ExecutorState IO)

executeTasks :: ExecutorCfg -> [Task] -> IO ()
executeTasks cfg tasks = do
  let initialState = ExecutorState{layoutCache = HM.empty}
  evalStateT (runReaderT (mapM_ executeTask tasks) cfg) initialState

executeTask :: (MonadReader ExecutorCfg m, MonadState ExecutorState m, MonadIO m) => Task -> m ()
executeTask task = undefined

getLayout :: (MonadReader ExecutorCfg m, MonadState ExecutorState m, MonadIO m) => RelPath -> m (Maybe Layout)
getLayout path = do
  cache <- gets layoutCache
  join
    <$> runMaybeT
      ( MaybeT (pure $ HM.lookup path cache) <|> do
          let parentDir = takeDirectory path
          guard (parentDir /= ".")
          parentLayout <- lift $ getLayout parentDir
          hasLayout <- asks (HS.member path . layouts)
          layout <- whenMaybe hasLayout $ do
            layoutDir <- asks layoutDir
            let layoutPath = layoutDir </> path
            liftIO $ readLayout layoutPath
          modify' $ \s -> s{layoutCache = HM.insert path layout $ layoutCache s}
          return $ parentLayout <> layout
      )

executeAction :: (MonadReader ExecutorCfg m, MonadIO m) => Task -> m ()
executeAction Task{..} = undefined
