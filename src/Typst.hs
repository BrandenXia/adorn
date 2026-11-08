module Typst (compileTypst, readDependencies, queryDependencies, queryMetadata) where

import Control.Monad (when)
import Data.Aeson (Object, eitherDecodeStrict')
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text.Encoding as TE
import Control.Monad.Except (ExceptT, liftEither, runExceptT)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Path
import System.Directory (canonicalizePath)
import System.IO.Extra (withTempDir)
import qualified System.FilePath as FP
import System.Exit (ExitCode (..))
import System.Process.Extra (readProcessWithExitCode)

import BuildError

compileTypst :: Path Abs File -> FilePath -> [String] -> FilePath -> ExceptT BuildError IO ()
compileTypst source target flags depsFile = do
  (code, stdout, stderr) <- buildIO "run Typst" (Just $ toFilePath source) $
    readProcessWithExitCode "typst"
      (["compile", "--deps", depsFile, "--deps-format", "zero"] ++ flags ++ [toFilePath source, target]) ""
  case code of
    ExitFailure n -> failBuild "compile Typst" (Just $ toFilePath source) $
      "Exit code " ++ show n ++ "\n" ++ stderr ++ stdout
    ExitSuccess -> do
      dependencies <- readDependencies depsFile
      when (null dependencies) $
        failBuild "read Typst dependencies" (Just depsFile) "The compiler reported no inputs."
      absolute <- buildIO "resolve Typst dependencies" (Just depsFile) $
        mapM canonicalizePath dependencies
      buildIO "write Typst dependencies" (Just depsFile) $
        TIO.writeFile depsFile (T.intercalate (T.singleton '\0') $ map T.pack absolute)

readDependencies :: FilePath -> ExceptT BuildError IO [FilePath]
readDependencies path = do
  contents <- buildIO "read Typst dependencies" (Just path) $ TIO.readFile path
  pure $ map T.unpack $ filter (not . T.null) $ T.splitOn (T.singleton '\0') contents

queryDependencies :: Path Abs File -> [String] -> ExceptT BuildError IO [FilePath]
queryDependencies source flags = do
  result <- buildIO "query Typst dependencies" (Just $ toFilePath source) $
    withTempDir $ \directory -> runExceptT $ do
      let deps = directory FP.</> "dependencies"
      compileTypst source (directory FP.</> "output") flags deps
      readDependencies deps
  liftEither result

queryMetadata :: Path Abs File -> [String] -> ExceptT BuildError IO Object
queryMetadata source flags = do
  let path = toFilePath source
  (code, stdout, stderr) <- buildIO "query Typst metadata" (Just path) $
    readProcessWithExitCode "typst"
      (["eval", "query(<adorn>).map(it => it.value)", "--in", path, "--format", "json"] ++ flags) ""
  case code of
    ExitFailure n -> failBuild "query Typst metadata" (Just path) $
      "Exit code " ++ show n ++ "\n" ++ stderr ++ stdout
    ExitSuccess -> do
      values <- either (failBuild "decode Typst metadata" (Just path)) pure $
        eitherDecodeStrict' $ TE.encodeUtf8 $ T.pack stdout
      case values of
        [] -> pure KM.empty
        [metadata] -> pure metadata
        _ -> failBuild "query Typst metadata" (Just path) $
          "Expected at most one <adorn> metadata declaration; found " ++ show (length values) ++ "."
