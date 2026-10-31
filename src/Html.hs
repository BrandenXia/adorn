{-# LANGUAGE OverloadedStrings #-}

module Html (extractTag, appendStyle, setTitle) where

import Data.Text (Text)
import Text.HTML.TagSoup

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
    _ -> insertStyle tags css
 where
  insertStyle ts c =
    let style = [TagOpen "style" [], TagText c, TagClose "style"]
     in case break (== TagClose "head") ts of
          (before, closeHead : after) ->
            before ++ style ++ (closeHead : after)
          _ -> ts
