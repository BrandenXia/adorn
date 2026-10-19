import qualified Data.Text.IO as TIO
import Mapping

mappingFile :: FilePath
mappingFile = "adorn.mapping"

main :: IO ()
main = do
  mappingsStr <- TIO.readFile mappingFile
  case parseMappings mappingsStr mappingFile of
    Left err -> putStr err
    Right mappings -> print mappings

  return ()
