{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Index (indexContext, entryURL) where

import qualified Data.ByteString as BS
import Data.Char (chr, isAlphaNum, isAscii, toLower)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Path
import System.FilePath (dropTrailingPathSeparator, splitDirectories, takeBaseName, takeExtension)
import Text.Mustache (object, toMustache, (~>))
import Text.Mustache.Types (Value)
import Text.Printf (printf)

import Html (documentTitle)
import Task (IndexEntry (..))

indexContext :: Path Rel File -> [IndexEntry] -> IO (HashMap Text Value)
indexContext index entries = do
  values <- mapM entryContext entries
  pure $ HM.singleton "entries" (toMustache values)
 where
  entryContext IndexEntry{..} = do
    title <-
      if map toLower (takeExtension $ toFilePath entryTarget) `elem` [".html", ".htm"]
        then documentTitle <$> TIO.readFile (toFilePath entryOutput)
        else pure ""
    let label = if T.null title then T.pack (takeBaseName $ toFilePath entrySource) else title
    pure $ object
      [ "url" ~> entryURL index entryTarget
      , "title" ~> label
      , "source" ~> T.pack (toFilePath entrySource)
      , "target" ~> T.pack (toFilePath entryTarget)
      ]

entryURL :: Path Rel File -> Path Rel File -> Text
entryURL index target =
  let (up, down) = dropCommon (segments $ parent index) (segments target)
   in T.intercalate "/" $ replicate (length up) ".." ++ map encodeSegment down
 where
  segments :: Path Rel t -> [String]
  segments = filter (`notElem` [".", ""]) . map dropTrailingPathSeparator . splitDirectories . toFilePath
  dropCommon (x : xs) (y : ys) | x == y = dropCommon xs ys
  dropCommon xs ys = (xs, ys)

encodeSegment :: String -> Text
encodeSegment = T.pack . concatMap encodeByte . BS.unpack . TE.encodeUtf8 . T.pack
 where
  encodeByte byte
    | isAscii char && (isAlphaNum char || char `elem` ['-', '.', '_', '~']) = [char]
    | otherwise = printf "%%%02X" byte
   where
    char = chr $ fromIntegral byte
