{-# LANGUAGE OverloadedStrings #-}

module Html (HtmlDocument (..), parseDocument, renderDocument, documentBody, documentTitle, mergeHead) where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree

data HtmlDocument = HtmlDocument
  { documentSource :: Text
  , documentHead :: [TagTree Text]
  , documentShell :: [TagTree Text]
  }

parseDocument :: Text -> HtmlDocument
parseDocument source = HtmlDocument source (concatMap heads shell) shell
 where
  shell = tagTree $ canonicalizeTags $ parseTags source
  heads (TagBranch "head" _ children) = children
  heads (TagBranch "html" _ children) = concatMap heads children
  heads _ = []

documentBody :: HtmlDocument -> Text
documentBody doc
  | not $ any wrapper $ documentShell doc = documentSource doc
  | otherwise = renderTree $ concatMap body $ documentShell doc
 where
  wrapper (TagBranch name _ _) = name `elem` ["html", "head", "body"]
  wrapper (TagLeaf (TagOpen name _)) = T.toLower name == "!doctype"
  wrapper _ = False
  body (TagBranch "html" _ children) = concatMap body children
  body (TagBranch "body" _ children) = children
  body (TagBranch "head" _ _) = []
  body (TagLeaf (TagOpen name _)) | T.toLower name == "!doctype" = []
  body node = [node]

documentTitle :: Text -> Text
documentTitle = innerText . takeWhile (/= TagClose "title") . drop 1
  . dropWhile (not . isTagOpenName "title") . canonicalizeTags . parseTags

mergeHead :: [TagTree Text] -> [TagTree Text] -> [TagTree Text]
mergeHead outer inner = filter keep outer ++ inner
 where
  keys = mapMaybe key inner
  keep node = maybe True (`notElem` keys) $ key node
  key :: TagTree Text -> Maybe (Text, Text)
  key (TagBranch "title" _ _) = Just ("title", "")
  key (TagLeaf (TagOpen "meta" attrs)) = case lookup "name" attrs of
    Just name -> Just ("name", T.toCaseFold name)
    Nothing -> (,) "property" <$> lookup "property" attrs
  key _ = Nothing

renderDocument :: HtmlDocument -> Text
renderDocument (HtmlDocument source [] _) = source
renderDocument (HtmlDocument _ headNodes shell) = renderTree $
  case break isHtml shell of
    (before, TagBranch name attrs children : after) ->
      before ++ [TagBranch name attrs (insertHead children)] ++ after
    _ -> insertHead shell
 where
  isHtml (TagBranch "html" _ _) = True
  isHtml _ = False
  isHead (TagBranch "head" _ _) = True
  isHead _ = False
  insertHead nodes = case break isHead nodes of
    (before, TagBranch name attrs _ : after) ->
      before ++ [TagBranch name attrs headNodes] ++ filter (not . isHead) after
    _ -> let (before, after) = span prologue nodes
          in before ++ [TagBranch "head" [] headNodes] ++ after
  prologue (TagLeaf (TagText text)) = T.null $ T.strip text
  prologue (TagLeaf (TagComment _)) = True
  prologue (TagLeaf (TagOpen name _)) = T.isPrefixOf "!" name || T.isPrefixOf "?" name
  prologue _ = False
