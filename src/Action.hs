module Action (Action (..), deps) where

import Path (File, SomeBase)

data Action
  = Typst {template :: Maybe (SomeBase File), bodyOnly :: Maybe Bool}
  | TypstPDF
  | Copy
  | Ignore
  deriving (Show, Eq)

deps :: Action -> [SomeBase File]
deps (Typst (Just t) _) = [t]
deps _ = []
