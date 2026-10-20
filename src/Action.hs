module Action (Action (..), deps) where

import Path (File, SomeBase)

data Action
  = Typst {template :: Maybe (SomeBase File), bodyOnly :: Maybe Bool}
  | TypstPDF {template :: Maybe (SomeBase File)}
  | Copy
  | Ignore
  deriving (Show, Eq)

deps :: Action -> [SomeBase File]
deps (Typst (Just t) _) = [t]
deps (TypstPDF (Just t)) = [t]
deps _ = []
