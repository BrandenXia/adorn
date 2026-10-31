{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module BuildError (BuildError (..), failBuild, buildIO, forTarget, renderBuildError) where

import Control.Exception (IOException, displayException, try)
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)

data BuildError = BuildError
  { errorOperation :: String
  , errorPath :: Maybe FilePath
  , errorTarget :: Maybe FilePath
  , errorDetails :: String
  }
  deriving (Show, Eq)

failBuild :: (MonadError BuildError m) => String -> Maybe FilePath -> String -> m a
failBuild operation path details = throwError $ BuildError operation path Nothing details

buildIO :: (MonadIO m, MonadError BuildError m) => String -> Maybe FilePath -> IO a -> m a
buildIO operation path action =
  liftIO (try @IOException action) >>= either (failBuild operation path . displayException) pure

forTarget :: (MonadError BuildError m) => FilePath -> m a -> m a
forTarget target action = catchError action $ \err -> throwError err{errorTarget = Just target}

renderBuildError :: BuildError -> String
renderBuildError (BuildError operation path target details) =
  "Error: " ++ operation
    ++ maybe "" (\p -> " [" ++ p ++ "]") path
    ++ maybe "" (\p -> " while building " ++ p) target
    ++ ":\n" ++ details
