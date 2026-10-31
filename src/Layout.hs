{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Layout (toLayoutFile, LayoutSet, listLayouts, Layout (..), readLayout, toLayout, applyLayout, getLayout, getFileLayout) where

import Control.Applicative ((<|>))
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
import Text.Mustache (Template, ToMustache, compileTemplate, substitute, toMustache)
import Text.Mustache.Types (Value (String))
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

-- Keep templates separate so every layer sees the same page context and
-- rendered child HTML is never parsed as another template.
newtype Layout = Layout [Template] deriving (Show)

readLayout :: FilePath -> IO Layout
readLayout path = do
  source <- TIO.readFile path
  either (ioError . userError . show) (pure . Layout . pure) $
    compileTemplate path source

toLayout :: Text -> Either String Layout
toLayout source =
  either (Left . show) (Right . Layout . pure) $ compileTemplate "layout" source

applyLayout :: (ToMustache value) => Layout -> HashMap Text value -> Text
applyLayout (Layout templates) context = foldr render child templates
 where
  values = HM.map toMustache context
  child = case HM.lookup "child" values of
    Just (String content) -> content
    _ -> ""
  render tpl content = substitute tpl (HM.insert "child" (toMustache content) values)

instance Semigroup Layout where
  Layout outer <> Layout inner = Layout (outer ++ inner)

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
