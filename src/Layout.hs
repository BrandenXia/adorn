{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Layout (toLayoutFile, LayoutSet, listLayouts, Layout (..), readLayout, toLayout, applyLayout, getLayout, getFileLayout) where

import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import Control.Monad.Extra (join, whenMaybe)
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Bool (bool)
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
import System.Directory (doesDirectoryExist)
import System.Directory.Extra (listFilesRecursive)

toLayoutFile :: Path Rel Dir -> Path Rel File
toLayoutFile dir = dir </> [relfile|layout.html|]

type LayoutSet = HashSet (Path Rel Dir)

listLayouts :: Path Abs Dir -> IO LayoutSet
listLayouts layoutDir =
  HS.fromList <$> do
    exists <- not <$> doesDirectoryExist (toFilePath layoutDir)
    files <- bool (listFilesRecursive (toFilePath layoutDir)) (pure []) exists >>= mapM parseAbsFile
    let layouts = filter ((== [relfile|layout.html|]) . filename) files
    map parent <$> mapM (stripProperPrefix layoutDir) layouts

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
            let layoutPath = layoutDir </> toLayoutFile path
            liftIO $ readLayout $ toFilePath layoutPath
          let composed = parentLayout <> layout
          modify' $ \s -> HM.insert path composed s
          return composed
      )

getFileLayout :: LayoutSet -> Path Rel File -> [Path Rel Dir]
getFileLayout set relFile = filter (`HS.member` set) $ getPossibleLayouts relFile

getPossibleLayouts :: Path Rel File -> [Path Rel Dir]
getPossibleLayouts = reverse . go . parent
 where
  go dir
    | parent dir == dir = [dir]
    | otherwise = dir : go (parent dir)
