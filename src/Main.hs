{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (guard)
import Control.Monad.Extra (whenM)
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import System.Console.CmdArgs
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, makeAbsolute)
import System.Exit (die, exitFailure)
import System.IO (hPutStr, stderr)

import Mapping

import Paths_adorn (version)

data Adorn = Adorn
  { mapping :: FilePath
  , out :: FilePath
  , base :: FilePath
  , layout :: FilePath
  }
  deriving (Show, Data, Typeable)

adorn =
  Adorn
    { mapping = "adorn.mapping" &= help "Mapping file" &= typFile
    , out = "dist" &= help "Output dir" &= typDir
    , base = "." &= help "Base dir" &= typDir
    , layout = "layout" &= help "Layout dir" &= typDir
    }
    &= summary ("Adorn v" ++ showVersion version ++ ", (C) Branden Xia 2026")

dieNoNewline msg = do
  hPutStr stderr msg
  exitFailure

main :: IO ()
main = do
  Adorn{..} <- cmdArgs adorn

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

  return ()
