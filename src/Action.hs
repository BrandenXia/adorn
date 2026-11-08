{-# LANGUAGE TypeFamilies #-}

module Action (Unresolved, Resolved, Action (..), IndexSpec (..), IndexSelection (..), Selection, compileSelection, matchesSelection, resolveAction, resolveIndex) where

import Data.Text (Text)
import Path
import Query (Query)
import Script (ScriptMode)
import Text.Regex.TDFA (Regex, makeRegexM, matchTest)

data Unresolved
data Resolved

type family ActionFilePath status where
  ActionFilePath Unresolved = SomeBase
  ActionFilePath Resolved = Path Abs

data Action status
  = Typst {template :: Maybe (ActionFilePath status File), bodyOnly :: Maybe Bool}
  | Index (IndexSpec status)
  | TypstPDF
  | Copy
  | Compose
  | Ignore
  | Choose ScriptMode (ActionFilePath status File) [(Text, (Action status, String))]

data IndexSpec status = IndexSpec
  { indexTemplate :: ActionFilePath status File
  , indexSelections :: [(Text, IndexSelection)]
  }

data IndexSelection = IndexSelection
  { selection :: Selection
  , selectionQuery :: Query
  }

newtype Selection = Selection Regex

compileSelection :: String -> Maybe Selection
compileSelection pattern = Selection <$> makeRegexM ("\\`" ++ pattern ++ "\\'")

matchesSelection :: Selection -> FilePath -> Bool
matchesSelection (Selection regex) = matchTest regex

resolveAction :: Path Abs Dir -> Action Unresolved -> Action Resolved
resolveAction cwd (Typst tpl fragment) = Typst (resolveFilePath cwd <$> tpl) fragment
resolveAction cwd (Choose mode script branches) =
  Choose mode (resolveFilePath cwd script) [(name, (resolveAction cwd act, dst)) | (name, (act, dst)) <- branches]
resolveAction cwd (Index spec) = Index (resolveIndex cwd spec)
resolveAction _ TypstPDF = TypstPDF
resolveAction _ Copy = Copy
resolveAction _ Compose = Compose
resolveAction _ Ignore = Ignore

resolveIndex :: Path Abs Dir -> IndexSpec Unresolved -> IndexSpec Resolved
resolveIndex cwd (IndexSpec tpl selected) = IndexSpec (resolveFilePath cwd tpl) selected

resolveFilePath :: Path Abs Dir -> SomeBase File -> Path Abs File
resolveFilePath _ (Abs path) = path
resolveFilePath cwd (Rel path) = cwd </> path
