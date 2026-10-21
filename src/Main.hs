{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (forM_, unless, when)
import Control.Monad.Extra (filterM, whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Bool (bool)
import Data.List (intercalate)
import Data.Maybe (isNothing)
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import Path
import System.Console.CmdArgs
import System.Directory
import System.Directory.Extra (listFilesRecursive)
import System.Exit (die, exitFailure)
import System.IO (hPutStr, stderr)

import Executor
import Mapping
import Task

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

dieNoNewline msg = do
  hPutStr stderr msg
  exitFailure

main :: IO ()
main = do
  checkRequirements

  Adorn{..} <- cmdArgs adorn

  when (workDir /= ".") $ do
    workDir <- canonicalizePath workDir >>= parseAbsDir
    whenM (not <$> doesDirectoryExist (toFilePath workDir)) $
      die ("Error: Working dir `" ++ toFilePath workDir ++ "` does not exist or is not a directory.")
    setCurrentDirectory (toFilePath workDir)

  base <- canonicalizePath base >>= parseAbsDir
  buildDir <- makeAbsolute buildDir >>= parseAbsDir
  out <- makeAbsolute out >>= parseAbsDir
  layout <- makeAbsolute layout >>= parseAbsDir
  mapping <- makeAbsolute mapping >>= parseAbsFile

  whenM (not <$> doesFileExist (toFilePath mapping)) $
    die ("Error: Mapping file `" ++ toFilePath mapping ++ "` does not exist.")
  whenM (not <$> doesDirectoryExist (toFilePath base)) $
    die ("Error: Base dir `" ++ toFilePath base ++ "` does not exist or is not a directory.")

  whenM (not <$> doesDirectoryExist (toFilePath layout)) $
    putStrLn ("Warning: Layout dir `" ++ toFilePath layout ++ "` does not exist or is not a directory.")

  mappingsStr <- TIO.readFile (toFilePath mapping)
  mapping <- case parseMappings mappingsStr (toFilePath mapping) of
    Left err -> do
      putStrLn "Error parsing mapping file:"
      dieNoNewline err
    Right mappings -> return mappings

  allFiles <- listFilesRecursive (toFilePath base) >>= mapM parseAbsFile
  let tasks = discoverTask base out mapping allFiles
  runExceptT
    ( forM_ tasks $ \task -> do
        failures <- liftIO $ taskDepsExist task
        unless (null failures) $ throwE failures
    )
    >>= \case
      Left failures ->
        die $ "Error: " ++ intercalate ", " (map show failures) ++ " does not exist."
      Right _ -> return ()
  tasks <- bool (filterM taskNecessary) pure full tasks

  createDirectoryIfMissing True (toFilePath buildDir)

  layouts <- listLayouts layout
  success <-
    executeTasks
      tasks
      ExecutorCfg
        { buildDir = buildDir
        , layoutDir = layout
        , layouts = layouts
        }

  unless success $ do
    die "Error: One or more tasks failed to execute."

checkRequirements :: IO ()
checkRequirements =
  let
    cli tool =
      whenM (isNothing <$> findExecutable tool) $
        die ("Error: Required tool `" ++ tool ++ "` is not installed or not in PATH.")
   in
    mapM_ cli ["typst"]
