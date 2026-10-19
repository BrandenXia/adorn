{-# LANGUAGE RecordWildCards #-}

module Task (Task, discoverTask, taskNecessary) where

import Data.Array (Array, bounds, inRange, (!))
import Data.Char (isDigit)
import Data.List.Extra (firstJust)
import Data.Maybe (mapMaybe)
import System.Directory (getModificationTime)
import System.Directory.Extra (doesFileExist)
import System.FilePath (makeRelative, normalise, (</>))
import Text.Read (readMaybe)
import Text.Regex.TDFA

import Action (Action, deps)
import Mapping (Mapping (..))

type BasePath = FilePath
type OutPath = FilePath
type AbsPath = FilePath
type RelPath = FilePath

data Task = Task
  { source :: AbsPath
  , target :: AbsPath
  , action :: Action
  }
  deriving (Show, Eq)

discoverTask :: BasePath -> OutPath -> [Mapping] -> [AbsPath] -> [Task]
discoverTask = ((mapMaybe .) .) . discoverFileTask

discoverFileTask :: BasePath -> OutPath -> [Mapping] -> AbsPath -> Maybe Task
discoverFileTask base out mappings abs =
  firstJust
    (applyMapping base out abs $ makeRelative base abs)
    mappings

applyMapping :: BasePath -> OutPath -> AbsPath -> RelPath -> Mapping -> Maybe Task
applyMapping base out abs rel Mapping{..} =
  case matchOnceText srcPat rel of
    Nothing -> Nothing
    Just (_, groups, _) ->
      let target = normalise $ out </> substitute groups dstTpl
       in Just $ Task abs target action

substitute :: Array Int (String, (Int, Int)) -> String -> String
substitute groups = go
 where
  go [] = []
  go ('$' : '$' : xs) = '$' : go xs
  go ('$' : xs) =
    let (digits, rest) = span isDigit xs
     in case readMaybe digits of
          Just n | inRange (bounds groups) n -> fst (groups ! n) ++ go rest
          _ -> '$' : go xs
  go (x : xs) = x : go xs

taskNecessary :: Task -> IO Bool
taskNecessary Task{..} = do
  mtimes <- mapM getModificationTime (source : deps action)
  targetExists <- doesFileExist target
  if not targetExists
    then return True
    else do
      targetMtime <- getModificationTime target
      return $ any (> targetMtime) mtimes
