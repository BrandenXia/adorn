{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Action (Unresolved, Resolved, Action (..), actionDeps, resolveAction) where

import Path

data Unresolved
data Resolved

type family ActionFilePath status where
  ActionFilePath Unresolved = SomeBase
  ActionFilePath Resolved = Path Abs

data Action status
  = Typst {template :: Maybe (ActionFilePath status File), bodyOnly :: Maybe Bool}
  | TypstPDF
  | Copy
  | Ignore

deriving instance (Show (ActionFilePath status File)) => Show (Action status)
deriving instance (Eq (ActionFilePath status File)) => Eq (Action status)

actionDeps :: Action Resolved -> [Path Abs File]
actionDeps (Typst (Just t) _) = [t]
actionDeps _ = []

resolveAction :: Path Abs Dir -> Action Unresolved -> Action Resolved
resolveAction cwd (Typst template bodyOnly) =
  Typst (resolveFilePath cwd <$> template) bodyOnly
resolveAction _ TypstPDF = TypstPDF
resolveAction _ Copy = Copy
resolveAction _ Ignore = Ignore

resolveFilePath :: Path Abs Dir -> SomeBase File -> Path Abs File
resolveFilePath _ (Abs path) = path
resolveFilePath cwd (Rel path) = cwd </> path
