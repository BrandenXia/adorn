{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Layout (Layout (..), readLayout, toLayout, applyLayout, getLayout) where

import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import Control.Monad.Extra (join, whenMaybe)
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Data.Text.Template (Template)
import qualified Data.Text.Template as Tpl
import Path

newtype Layout = Layout Template deriving (Show, Eq)

readLayout :: FilePath -> IO Layout
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

type LayoutCache = HashMap (Path Rel Dir) (Maybe Layout)

getLayout ::
  (MonadState LayoutCache m, MonadIO m) =>
  Path Abs Dir ->
  HashSet (Path Rel Dir) ->
  Path Rel Dir ->
  m (Maybe Layout)
getLayout layoutDir layouts path = do
  cache <- get
  join
    <$> runMaybeT
      ( MaybeT (pure $ HM.lookup path cache) <|> do
          let parentDir = parent path
          parentLayout <-
            if parentDir == path
              then pure Nothing
              else lift $ getLayout layoutDir layouts parentDir
          let hasLayout = HS.member path layouts
          layout <- whenMaybe hasLayout $ do
            let layoutPath = layoutDir </> path </> [relfile|layout.html|]
            liftIO $ readLayout $ toFilePath layoutPath
          let composed = parentLayout <> layout
          modify' $ \s -> HM.insert path composed s
          return composed
      )
