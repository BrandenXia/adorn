{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Index (indexContext, entryURL) where

import Control.Monad (forM)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
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
import Text.Mustache (toMustache)
import Text.Mustache.Types (Value)
import Text.Printf (printf)

import Html (documentTitle)
import Task (IndexEntry (..))
import Query (Query, applyQuery)

indexContext :: Path Rel File -> [(Text, Query, [IndexEntry])] -> IO (HashMap Text Value)
indexContext index selections = HM.fromList <$> forM selections (\(name, query, entries) -> do
  values <- mapM entryValue entries
  selected <- either (ioError . userError . (("Index selection " ++ T.unpack name ++ ": ") ++)) pure $
    applyQuery query values
  pure (name, toMustache selected))
 where
  entryValue IndexEntry{..} = do
    title <-
      if map toLower (takeExtension $ toFilePath entryTarget) `elem` [".html", ".htm"]
        then documentTitle <$> TIO.readFile (toFilePath entryOutput)
        else pure ""
    let label = case KM.lookup "title" entryContext of
          Just (A.String value) -> value
          _ -> if T.null title then T.pack (takeBaseName $ toFilePath entrySource) else title
    pure $ object
      [ "meta" .= A.Object entryContext
      , "url" .= entryURL index entryTarget
      , "title" .= label
      , "source" .= T.pack (toFilePath entrySource)
      , "target" .= T.pack (toFilePath entryTarget)
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
