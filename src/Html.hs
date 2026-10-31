{-# LANGUAGE OverloadedStrings #-}

module Html (HtmlDocument (..), parseDocument, renderDocument, documentTitle) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Text.HTML.TagSoup

import Layout (Layout, applyLayout)

data HtmlDocument = HtmlDocument
  { documentSource :: Text
  , documentBody :: Text
  , documentCss :: Text
  , documentName :: Text
  }

parseDocument :: Text -> HtmlDocument
parseDocument source =
  let tags = parseTags source
   in HtmlDocument source (extractTag "body" tags) (extractTag "style" tags) (titleFromTags tags)

documentTitle :: Text -> Text
documentTitle = titleFromTags . parseTags

titleFromTags :: [Tag Text] -> Text
titleFromTags = innerText . takeWhile (/= TagClose "title") . drop 1 . dropWhile (not . isTagOpenName "title")

renderDocument :: Maybe Layout -> HtmlDocument -> Text
renderDocument Nothing doc = documentSource doc
renderDocument layout doc =
  let title = documentName doc
      context = HM.fromList $ ("child", documentBody doc) : [("title", title) | not (T.null title)]
      styled = appendStyle (parseTags $ applyLayout layout context) (documentCss doc)
      titled = if T.null title then styled else setTitle styled title
   in renderTags titled

extractTag :: Text -> [Tag Text] -> Text
extractTag name tags =
  let start = drop 1 $ dropWhile (~/= TagOpen name []) tags
      content = takeWhile (~/= TagClose name) start
   in renderTags content

setTitle :: [Tag Text] -> Text -> [Tag Text]
setTitle tags title =
  case break (isTagOpenName "head") tags of
    (before, openHead : rest) ->
      case break (== TagClose "head") rest of
        (headTags, closeHead : after) ->
          before ++ [openHead] ++ removeTitles headTags ++ titleTags ++ (closeHead : after)
        _ -> tags
    _ -> tags
 where
  titleTags = [TagOpen "title" [], TagText title, TagClose "title"]
  removeTitles [] = []
  removeTitles (tag : rest)
    | isTagOpenName "title" tag =
        removeTitles $ drop 1 $ dropWhile (/= TagClose "title") rest
    | otherwise = tag : removeTitles rest

appendStyle :: [Tag Text] -> Text -> [Tag Text]
appendStyle tags css =
  case break (== TagClose "style") tags of
    (before, closeStyle : after) ->
      before ++ [TagText ("\n" <> css)] ++ (closeStyle : after)
    _ -> case break (== TagClose "head") tags of
      (before, closeHead : after) ->
        before ++ [TagOpen "style" [], TagText css, TagClose "style"] ++ (closeHead : after)
      _ -> tags
