{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Layout (toLayoutFile, LayoutSet, listLayouts, Layout, readLayout, applyLayout, getFileLayout) where

import Data.Maybe (fromMaybe)
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
listLayouts layoutDir = do
  exists <- doesDirectoryExist (toFilePath layoutDir)
  files <- if exists then listFilesRecursive (toFilePath layoutDir) else pure []
  paths <- mapM parseAbsFile files
  let layouts = filter ((== [relfile|layout.html|]) . filename) paths
  HS.fromList . map parent <$> mapM (stripProperPrefix layoutDir) layouts

newtype Layout = Layout [Template] deriving (Show)

readLayout :: FilePath -> IO Layout
readLayout path = do
  source <- TIO.readFile path
  either (ioError . userError . show) (pure . Layout . pure) $
    compileTemplate path source

applyLayout :: (ToMustache value) => Maybe Layout -> HashMap Text value -> Text
applyLayout layout context = foldr render child templates
 where
  Layout templates = fromMaybe mempty layout
  values = HM.map toMustache context
  child = case HM.lookup "child" values of
    Just (String content) -> content
    _ -> ""
  render tpl content = substitute tpl (HM.insert "child" (toMustache content) values)

instance Semigroup Layout where
  Layout outer <> Layout inner = Layout (outer ++ inner)

instance Monoid Layout where
  mempty = Layout []

getFileLayout :: LayoutSet -> Path Rel File -> [Path Rel Dir]
getFileLayout set relFile = filter (`HS.member` set) $ getPossibleLayouts relFile

getPossibleLayouts :: Path Rel File -> [Path Rel Dir]
getPossibleLayouts = reverse . go . parent
 where
  go dir
    | parent dir == dir = [dir]
    | otherwise = dir : go (parent dir)
