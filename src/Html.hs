{-# LANGUAGE OverloadedStrings #-}

module Html (extractTag, appendStyle) where

import Data.Text (Text)
import Text.HTML.TagSoup

extractTag :: Text -> [Tag Text] -> Text
extractTag name tags =
  let start = drop 1 $ dropWhile (~/= TagOpen name []) tags
      content = takeWhile (~/= TagClose name) start
   in renderTags content

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
