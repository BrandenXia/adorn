{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad.Extra (filterM, whenM)
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import System.Console.CmdArgs
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, makeAbsolute, setCurrentDirectory)
import System.Exit (die, exitFailure)
import System.IO (hPutStr, stderr)

import Mapping
import Task

import Control.Monad (unless, when)
import Data.Bool (bool)
import Paths_adorn (version)
import System.Directory.Extra (listFilesRecursive)

data Adorn = Adorn
  { workDir :: FilePath
  , mapping :: FilePath
  , out :: FilePath
  , base :: FilePath
  , layout :: FilePath
  , full :: Bool
  }
  deriving (Show, Data, Typeable)

adorn =
  Adorn
    { workDir = "." &= help "Working dir" &= typDir &= name "C"
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
  Adorn{..} <- cmdArgs adorn

  when (workDir /= ".") $ do
    workDir <- canonicalizePath workDir
    whenM (not <$> doesDirectoryExist workDir) $
      die ("Error: Working dir `" ++ workDir ++ "` does not exist or is not a directory.")
    setCurrentDirectory workDir

  base <- canonicalizePath base
  out <- makeAbsolute out
  layout <- makeAbsolute layout

  whenM (not <$> doesFileExist mapping) $
    die ("Error: Mapping file `" ++ mapping ++ "` does not exist.")
  whenM (not <$> doesDirectoryExist base) $
    die ("Error: Base dir `" ++ base ++ "` does not exist or is not a directory.")

  whenM (not <$> doesDirectoryExist layout) $
    putStrLn ("Warning: Layout dir `" ++ layout ++ "` does not exist or is not a directory.")

  mappingsStr <- TIO.readFile mapping
  mapping <- case parseMappings mappingsStr mapping of
    Left err -> do
      putStrLn "Error parsing mapping file:"
      dieNoNewline err
    Right mappings -> return mappings

  allFiles <- listFilesRecursive base
  let tasks = discoverTask base out mapping allFiles
  tasks <- bool (filterM taskNecessary) pure full tasks
  print tasks

  return ()
