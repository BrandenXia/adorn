{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Maybe (isNothing)
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import Path
import System.Console.CmdArgs
import System.Directory
import System.Exit (die)

import BuildError
import Executor (executePlan)
import Layout (listLayouts)
import Mapping (parseMappings)
import Plan
import Paths_adorn (version)

data Adorn = Adorn
  { workDir :: String
  , buildDir :: String
  , mapping :: String
  , out :: String
  , base :: String
  , layout :: String
  , full :: Bool
  }
  deriving (Show, Data, Typeable)

adorn :: Adorn
adorn =
  Adorn
    { workDir = "." &= help "Working dir" &= typDir &= name "C"
    , buildDir = "build" &= help "Build dir" &= typDir &= name "B"
    , mapping = "adorn.mapping" &= help "Mapping file" &= typFile
    , out = "dist" &= help "Output dir" &= typDir
    , base = "." &= help "Base dir" &= typDir
    , layout = "layout" &= help "Layout dir" &= typDir
    , full = False &= help "Full build (ignore timestamps)"
    }
    &= summary ("Adorn v" ++ showVersion version ++ ", (C) Branden Xia 2026")

main :: IO ()
main = do
  options <- cmdArgs adorn
  runExceptT (runBuild options) >>= either (die . renderBuildError) pure

runBuild :: Adorn -> ExceptT BuildError IO ()
runBuild Adorn{..} = do
  when (workDir /= ".") $ buildIO "set working directory" (Just workDir) $ setCurrentDirectory workDir
  cwd <- buildIO "read working directory" Nothing getCurrentDirectory >>= resolveDir
  source <- resolveDir base
  output <- resolveDir out
  build <- resolveDir buildDir
  layoutRoot <- resolveDir layout
  absoluteMapping <- buildIO "resolve mapping file" (Just mapping) $ canonicalizePath mapping
  mappingPath <- maybe (failBuild "resolve mapping file" (Just mapping) "Expected a file path.") pure $
    parseAbsFile absoluteMapping

  sourceExists <- buildIO "check source directory" (Just base) $ doesDirectoryExist $ toFilePath source
  unless sourceExists $ failBuild "configure source directory" (Just base) "Directory does not exist."

  contents <- buildIO "read mappings" (Just $ toFilePath mappingPath) $ TIO.readFile $ toFilePath mappingPath
  rules <- either (failBuild "parse mappings" (Just $ toFilePath mappingPath)) pure $
    parseMappings contents (toFilePath mappingPath)
  layouts <- buildIO "discover layouts" (Just $ toFilePath layoutRoot) $ listLayouts layoutRoot
  let cfg = PlanCfg
        { workingDir = cwd
        , sourceDir = source
        , outputDir = output
        , buildDir = build
        , layoutRoot = layoutRoot
        , mappingFile = mappingPath
        , layoutSet = layouts
        , mappings = rules
        }
  plan <- buildPlan cfg full
  when (requiresTypst plan) $ do
    executable <- buildIO "find Typst" Nothing $ findExecutable "typst"
    when (isNothing executable) $ failBuild "check build tools" Nothing "Required tool `typst` is not installed or not in PATH."
  executePlan plan
 where
  resolveDir :: FilePath -> ExceptT BuildError IO (Path Abs Dir)
  resolveDir path = do
    absolute <- buildIO "resolve directory" (Just path) $ canonicalizePath path
    maybe (failBuild "resolve directory" (Just path) "Expected a directory path.") pure $ parseAbsDir absolute
