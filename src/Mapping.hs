{-# LANGUAGE OverloadedStrings #-}

module Mapping (Mapping (..), parseMappings) where

import Control.Applicative.Permutations (
  Permutation,
  runPermutation,
  toPermutation,
  toPermutationWithDefault,
 )
import Control.Monad (when)
import Data.Char (isSpace)
import Data.Functor (void, ($>))
import Data.List (dropWhileEnd)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Path (File, SomeBase, parseSomeFile)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Regex.TDFA (Regex, RegexMaker (makeRegexM))

import Action (Action (..))

data Mapping = Mapping
  { srcPat :: Regex
  , dstTpl :: String
  , action :: Action
  }

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space hspace1 empty empty

keyword :: Text -> Parser Text
keyword name = try (string name <* notFollowedBy alphaNumChar) <* sc

param :: Text -> Parser a -> Parser a
param name parse = do
  void $ string (name <> "=")
  val <- parse
  sc
  return val

optionalParam :: Text -> Parser a -> Permutation Parser (Maybe a)
optionalParam name parser =
  toPermutationWithDefault
    Nothing
    (Just <$> param name parser)

requiredParam :: Text -> Parser a -> Permutation Parser a
requiredParam name parser =
  toPermutation (param name parser)

someString :: Parser String
someString = manyTill L.charLiteral (lookAhead (void spaceChar <|> eof))

someBool :: Parser Bool
someBool = (string "true" $> True) <|> (string "false" $> False)

someFile :: Parser (SomeBase File)
someFile = do
  raw <- someString
  case parseSomeFile raw of
    Nothing -> fail ("Invalid template file path: " ++ raw)
    Just path -> pure path

pAction :: Parser Action
pAction =
  choice
    [ keyword "typst"
        *> runPermutation
          ( Typst
              <$> optionalParam "template" someFile
              <*> optionalParam "bodyOnly" someBool
          )
    , keyword "typstPDF"
        *> runPermutation
          ( TypstPDF
              <$> optionalParam "template" someFile
          )
    , keyword "copy" $> Copy
    , keyword "ignore" $> Ignore
    ]

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

pRegexAt :: Int -> String -> Parser Regex
pRegexAt start src = do
  result <- observing (makeRegexM $ "\\`" ++ src ++ "\\'")
  case result of
    Right regex -> pure regex
    Left err -> do
      setOffset start
      fail "Invalid regex pattern"

pMapping :: Parser Mapping
pMapping = do
  start <- getOffset
  src <- trim <$> manyTill anySingle (try (string "=>"))
  when (null src) $ fail "src cannot be empty"
  src <- pRegexAt start src
  sc
  action <- pAction
  sc
  void $ string "=>"
  sc
  dst <- trim <$> manyTill anySingle (lookAhead (void eol <|> eof))
  when (null dst) $ fail "dst cannot be empty"
  return $ Mapping src dst action

pMappings :: Parser [Mapping]
pMappings =
  catMaybes <$> many (notFollowedBy eof *> pLine) <* eof
 where
  pLine = do
    sc
    result <- optional (notFollowedBy (char '#') *> pMapping)
    sc
    optional (L.skipLineComment "#")
    void eol <|> eof
    pure result

parseMappings :: Text -> String -> Either String [Mapping]
parseMappings input srcFile =
  case runParser pMappings srcFile input of
    Left err -> Left (errorBundlePretty err)
    Right mappings -> Right mappings
