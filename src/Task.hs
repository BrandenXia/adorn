{-# LANGUAGE QuasiQuotes #-}

module Task (Task (..), Operation (..), IndexEntry (..), taskSource, taskNeedsTypst, taskIsIndex, taskSignature, taskCacheDir, taskCacheFile) where

import Data.Aeson (Object)
import Data.Text (Text)
import qualified Data.Text as T
import Path
import Query (Query)
import qualified System.FilePath as FP

data Task = Task
  { target :: Path Abs File
  , relative :: Path Rel File
  , operation :: Operation
  , taskDeps :: [Path Abs File]
  , taskLayouts :: [Path Abs File]
  , taskContext :: Object
  , taskOrigin :: String
  }
  deriving (Show, Eq)

data Operation
  = CopyFile (Path Abs File)
  | ComposeFile (Path Abs File)
  | CompileHtml (Path Abs File) Bool
  | CompilePdf (Path Abs File)
  | GenerateIndex [(Text, Query, [IndexEntry])]
  deriving (Show, Eq)

data IndexEntry = IndexEntry
  { entrySource :: Path Rel File
  , entryTarget :: Path Rel File
  , entryOutput :: Path Abs File
  , entryContext :: Object
  }
  deriving (Show, Eq)

taskSource :: Task -> Maybe (Path Abs File)
taskSource task = case operation task of
  CopyFile path -> Just path
  ComposeFile path -> Just path
  CompileHtml path _ -> Just path
  CompilePdf path -> Just path
  GenerateIndex _ -> Nothing

taskNeedsTypst :: Task -> Bool
taskNeedsTypst task = case operation task of
  CompileHtml{} -> True
  CompilePdf{} -> True
  _ -> False

taskIsIndex :: Task -> Bool
taskIsIndex task = case operation task of
  GenerateIndex{} -> True
  _ -> False

taskSignature :: Task -> Text
taskSignature = T.pack . ("head-composition-v1\n" ++) . show

taskCacheDir :: Path Abs Dir -> Task -> FilePath
taskCacheDir buildDir task = toFilePath (buildDir </> [reldir|.adorn/tasks|] </> relative task)

taskCacheFile :: Path Abs Dir -> Task -> FilePath -> FilePath
taskCacheFile buildDir task name = taskCacheDir buildDir task FP.</> name
