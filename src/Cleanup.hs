{-# LANGUAGE OverloadedStrings #-}

module Cleanup (finishChoices) where

import Control.Monad (forM_, unless, when)
import Control.Monad.Except (ExceptT)
import Data.Aeson (eitherDecodeStrict', encode, object, (.:), (.=))
import Data.Aeson.Types (parseEither, withObject)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Path
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import qualified System.FilePath as FP

import BuildError

finishChoices :: Path Abs Dir -> Path Abs Dir -> Path Abs File
              -> [(Path Abs File, Maybe (Path Abs File))] -> [Path Abs File]
              -> ExceptT BuildError IO ()
finishChoices build output mapping choices protected = unless (null choices) $ do
  let directory = toFilePath build FP.</> ".adorn"
      manifest = directory FP.</> "choices.json"
      root = toFilePath output
      config = toFilePath mapping
  exists <- buildIO "read selected outputs" (Just manifest) $ doesFileExist manifest
  previous <- if not exists then pure M.empty else do
    bytes <- buildIO "read selected outputs" (Just manifest) $ BS.readFile manifest
    (oldRoot, oldConfig, outputs) <- either (failBuild "read selected outputs" (Just manifest)) pure $
      eitherDecodeStrict' bytes >>= parseEither (withObject "selected outputs" $ \o ->
        (,,) <$> o .: "output" <*> o .: "mapping" <*> o .: "sources")
    pure $ if oldRoot == root && oldConfig == config then outputs else M.empty
  physicalProtected <- mapM (physical . toFilePath) protected
  forM_ choices $ \(source, selected) ->
    forM_ (M.lookup (toFilePath source) previous) $ \old -> do
      relative <- maybe (failBuild "read selected outputs" (Just manifest) "Invalid tracked output path.") pure $
        parseRelFile old
      let path = output </> relative
      when (Just path /= selected && path `notElem` protected) $ do
        present <- buildIO "inspect previous output" (Just $ toFilePath path) $ doesFileExist $ toFilePath path
        when present $ do
          resolved <- physical $ toFilePath path
          unless (resolved `elem` physicalProtected) $ do
            case stripProperPrefix output resolved of
              Nothing -> failBuild "remove previous output" (Just $ toFilePath path) "Tracked output resolves outside the output directory."
              Just _ -> buildIO "remove previous output" (Just $ toFilePath path) $ removeFile $ toFilePath path
  let current = M.fromList
        [(toFilePath source, toFilePath <$> (selected >>= stripProperPrefix output)) | (source, selected) <- choices]
      outputs = M.mapMaybe id current `M.union` M.difference previous current
      contents = object ["output" .= root, "mapping" .= config, "sources" .= outputs]
  buildIO "save selected outputs" (Just manifest) $ do
    createDirectoryIfMissing True directory
    BL.writeFile (manifest ++ ".tmp") $ encode contents
    renameFile (manifest ++ ".tmp") manifest
 where
  physical :: FilePath -> ExceptT BuildError IO (Path Abs File)
  physical path = buildIO "resolve build path" (Just path) $ canonicalizePath path >>= parseAbsFile
