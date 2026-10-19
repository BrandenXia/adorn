module Action (Action (..)) where

data Action
  = Typst {template :: Maybe FilePath, bodyOnly :: Maybe Bool}
  | TypstPDF {template :: Maybe FilePath}
  | Copy
  | Ignore
  deriving (Show, Eq)
