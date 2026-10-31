module Typst (compileTypst, readDependencies) where

import Control.Monad (when)
import Control.Monad.Except (ExceptT)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Path
import System.Directory (canonicalizePath)
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
