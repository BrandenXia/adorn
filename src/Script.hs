{-# LANGUAGE OverloadedStrings #-}

module Script (ScriptMode (..), runScript) where

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT)
import Data.Aeson (Object, Value (Object), eitherDecodeStrict', encode, object, (.:), (.:?), (.!=), (.=))
import Data.Aeson.Types (parseEither, withObject)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Path
import System.Exit (ExitCode (..))
import System.IO (stderr)
import System.Process.Extra (readProcessWithExitCode)

import BuildError

data ScriptMode = Selector | Predicate deriving (Show, Eq)

runScript :: ScriptMode -> Path Abs File -> Path Abs File -> Object
          -> ExceptT BuildError IO (Text, Object)
runScript mode executable source context = do
  let request = object ["source" .= toFilePath source, "context" .= Object context]
      input = T.unpack $ TE.decodeUtf8 $ BL.toStrict $ encode request
      path = toFilePath executable
  (code, output, diagnostic) <- buildIO "run script" (Just path) $
    readProcessWithExitCode path [] input
  when (not (null diagnostic) && (code == ExitSuccess || (mode == Predicate && code == ExitFailure 1))) $
    buildIO "report script diagnostics" (Just path) $ TIO.hPutStr stderr (T.pack diagnostic)
  case (mode, code) of
    (Predicate, ExitSuccess) -> pure ("yes", context)
    (Predicate, ExitFailure 1) -> pure ("no", context)
    (Selector, ExitSuccess) -> do
      (selected, updates) <- either (failBuild "decode script response" (Just path)) pure $
        eitherDecodeStrict' (TE.encodeUtf8 $ T.pack output) >>= parseEither response
      pure (selected, KM.union updates context)
    (_, ExitFailure n) -> failBuild "run script" (Just path) $
      "Exit code " ++ show n ++ if null diagnostic then "" else "\n" ++ diagnostic
 where
  response = withObject "selector response" $ \o -> do
    let unknown = filter (`notElem` ["choice", "context"]) (KM.keys o)
    unless (null unknown) $ fail ("Unknown response fields: " ++ show unknown)
    selected <- o .: "choice"
    updates <- o .:? "context" .!= KM.empty
    pure (selected, updates)
