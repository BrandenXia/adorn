{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Layout (LayoutSet, listLayouts, Layout, readLayout, applyLayout, getFileLayout) where

import Control.Monad (forM_, when)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Mustache (Template, ToMustache, compileTemplate, substitute, toMustache)
import Text.Mustache.Types (Value (String, Object))
import Text.HTML.TagSoup (Tag (TagText), innerText)
import Text.HTML.TagSoup.Tree (TagTree (TagBranch, TagLeaf), flattenTree)

import Path
import System.Directory (doesDirectoryExist)
import System.Directory.Extra (listFilesRecursive)

import Html (HtmlDocument (..), parseDocument, renderDocument, documentBody, mergeHead)

layoutNames :: [Path Rel File]
layoutNames = [[relfile|layout.html|], [relfile|layout.mustache|]]

type LayoutSet = HashSet (Path Rel File)

listLayouts :: Path Abs Dir -> IO LayoutSet
listLayouts layoutDir = do
  exists <- doesDirectoryExist (toFilePath layoutDir)
  files <- if exists then listFilesRecursive (toFilePath layoutDir) else pure []
  paths <- mapM parseAbsFile files
  layouts <- mapM (stripProperPrefix layoutDir) $ filter ((`elem` layoutNames) . filename) paths
  let directories = HM.fromListWith (++) [(parent path, [path]) | path <- layouts]
  forM_ (sort $ HM.toList directories) $ \(dir, matches) -> when (length matches > 1) $
    ioError $ userError $ "Ambiguous layout in " ++ toFilePath (layoutDir </> dir)
      ++ "; keep only one of layout.html and layout.mustache."
  pure $ HS.fromList layouts

newtype Layout = Layout [Template] deriving (Show)

readLayout :: FilePath -> IO Layout
readLayout path = do
  source <- TIO.readFile path
  either (ioError . userError . show) (pure . Layout . pure) $
    compileTemplate path source

applyLayout :: (ToMustache value) => Maybe Layout -> HashMap Text value -> Text
applyLayout layout context
  | null templates = child
  | otherwise = renderDocument $ foldr render initial templates
 where
  Layout templates = fromMaybe mempty layout
  values = HM.map toMustache context
  child = case HM.lookup "child" values of
    Just (String content) -> content
    _ -> ""
  parsed = parseDocument child
  initial = case HM.lookup "meta" values of
    Just (Object meta) | Just (String title) <- HM.lookup "title" meta ->
      parsed{documentHead = mergeHead (documentHead parsed) [TagBranch "title" [] [TagLeaf $ TagText title]]}
    _ -> parsed
  render tpl inner =
    let title = innerText $ flattenTree [node | node@(TagBranch "title" _ _) <- documentHead inner]
        variables = HM.insert "child" (toMustache $ documentBody inner) values
        titled = if T.null title then variables else HM.insert "title" (toMustache title) variables
        outer = parseDocument $ substitute tpl titled
     in outer{documentHead = mergeHead (documentHead outer) (documentHead inner)}

instance Semigroup Layout where
  Layout outer <> Layout inner = Layout (outer ++ inner)

instance Monoid Layout where
  mempty = Layout []

getFileLayout :: LayoutSet -> Path Rel File -> [Path Rel File]
getFileLayout set relFile =
  [path | dir <- getPossibleLayouts relFile, name <- layoutNames, let path = dir </> name, HS.member path set]

getPossibleLayouts :: Path Rel File -> [Path Rel Dir]
getPossibleLayouts = reverse . go . parent
 where
  go dir
    | parent dir == dir = [dir]
    | otherwise = dir : go (parent dir)
