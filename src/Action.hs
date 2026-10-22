module Action (Action (..), actionDeps) where

import Path (File, SomeBase)

data Action
  = Typst {template :: Maybe (SomeBase File), bodyOnly :: Maybe Bool}
  | TypstPDF
  | Copy
  | Ignore
  deriving (Show, Eq)

actionDeps :: Action -> [SomeBase File]
actionDeps (Typst (Just t) _) = [t]
actionDeps _ = []
