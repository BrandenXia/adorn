{-# LANGUAGE OverloadedStrings #-}

module Mapping (Action, Mapping, parseMappings) where

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
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Action
  = Typst {template :: Maybe FilePath, bodyOnly :: Maybe Bool}
  | TypstPDF {template :: Maybe FilePath}
  | Copy
  | Ignore
  deriving (Show, Eq)

data Mapping = Mapping
  { srcPat :: String
  , dstPat :: String
  , action :: Action
  }
  deriving (Show, Eq)

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

pAction :: Parser Action
pAction =
  choice
    [ keyword "typst"
        *> runPermutation
          ( Typst
              <$> optionalParam "template" someString
              <*> optionalParam "bodyOnly" someBool
          )
    , keyword "typstPDF"
        *> runPermutation
          ( TypstPDF
              <$> optionalParam "template" someString
          )
    , keyword "copy" $> Copy
    , keyword "ignore" $> Ignore
    ]

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

pMapping :: Parser Mapping
pMapping = do
  src <- trim <$> manyTill anySingle (try (string "=>"))
  when (null src) $ fail "src cannot be empty"
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
