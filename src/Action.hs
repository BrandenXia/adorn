module Action (Action (..), deps) where

data Action
  = Typst {template :: Maybe FilePath, bodyOnly :: Maybe Bool}
  | TypstPDF {template :: Maybe FilePath}
  | Copy
  | Ignore
  deriving (Show, Eq)

deps :: Action -> [FilePath]
deps (Typst (Just t) _) = [t]
deps (TypstPDF (Just t)) = [t]
deps _ = []
