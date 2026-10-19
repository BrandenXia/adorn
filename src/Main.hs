{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (guard)
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import System.Console.CmdArgs

import Mapping

import Paths_adorn (version)

data Adorn = Adorn
  { mapping :: FilePath
  , out :: FilePath
  , base :: FilePath
  }
  deriving (Show, Data, Typeable)

adorn =
  Adorn
    { mapping = "adorn.mapping" &= help "Mapping file" &= typFile
    , out = "dist" &= help "Output dir" &= typDir
    , base = "." &= help "Base dir" &= typDir
    }
    &= summary ("Adorn v" ++ showVersion version ++ ", (C) Branden Xia 2026")

main :: IO ()
main = do
  Adorn{..} <- cmdArgs adorn

  mappingsStr <- TIO.readFile mapping
  case parseMappings mappingsStr mapping of
    Left err -> putStr err
    Right mappings -> print mappings

  return ()
