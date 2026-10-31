{-# LANGUAGE OverloadedStrings #-}

module Cleanup (finishChoices) where

import Control.Monad (forM_, unless, when)
import Control.Monad.Except (ExceptT)
import Data.Aeson (eitherDecodeStrict', encode, object, (.:), (.=))
import Data.Aeson.Types (parseEither, withObject)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Path
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import qualified System.FilePath as FP

import BuildError

-- Only sources with a choice in this build participate in cleanup. Unmatched
-- sources and outputs from ordinary mappings retain their previous behavior.
-- Called after all outputs (including indexes) have been built successfully.
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
    value <- either (failBuild "read selected outputs" (Just manifest)) pure $ eitherDecodeStrict' bytes
    (oldRoot, oldConfig, outputs) <- either (failBuild "read selected outputs" (Just manifest)) pure $
      parseEither (withObject "selected outputs" $ \o ->
        (,,) <$> o .: "output" <*> o .: "mapping" <*> o .: "sources") value
    pure $ if oldRoot == root && oldConfig == config then outputs else M.empty
  -- Resolve protections as well as lexical paths: an old output may have become
  -- an input via a symlink since the previous successful build.
  physicalProtected <- mapM (physical . toFilePath) protected
  forM_ choices $ \(source, selected) ->
    case M.lookup (toFilePath source) previous of
      Nothing -> pure ()
      Just old -> do
        relative <- maybe (failBuild "read selected outputs" (Just manifest) "Invalid tracked output path.") pure $
          parseRelFile old
        let path = output </> relative
        when (Just path /= selected && path `notElem` protected) $ do
          present <- buildIO "inspect previous output" (Just $ toFilePath path) $ doesFileExist $ toFilePath path
          when present $ do
            resolved <- physical $ toFilePath path
            unless (resolved `elem` physicalProtected) $ do
              -- Never follow an output directory symlink outside the output root.
              case stripProperPrefix output resolved of
                Nothing -> failBuild "remove previous output" (Just $ toFilePath path) "Tracked output resolves outside the output directory."
                Just _ -> buildIO "remove previous output" (Just $ toFilePath path) $ removeFile $ toFilePath path
  let touched = map (toFilePath . fst) choices
      retained = foldr M.delete previous touched
      current = M.fromList $ catMaybes
        [ fmap (\path -> (toFilePath source, toFilePath path)) (selected >>= stripProperPrefix output)
        | (source, selected) <- choices
        ]
      contents = object ["output" .= root, "mapping" .= config, "sources" .= M.union current retained]
  buildIO "save selected outputs" (Just manifest) $ do
    createDirectoryIfMissing True directory
    BL.writeFile (manifest ++ ".tmp") $ encode contents
    renameFile (manifest ++ ".tmp") manifest
 where
  physical :: FilePath -> ExceptT BuildError IO (Path Abs File)
  physical path = buildIO "resolve build path" (Just path) $ canonicalizePath path >>= parseAbsFile
