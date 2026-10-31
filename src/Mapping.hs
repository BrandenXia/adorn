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
import Data.List (dropWhileEnd, nub, sort)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Path (File, SomeBase, parseSomeFile)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Regex.TDFA (Regex, RegexMaker (makeRegexM))

import Action
import Script (ScriptMode (..))

data Mapping = Mapping
  { srcPat :: Regex
  , dstTpl :: String
  , action :: Action Unresolved
  , mappingOrigin :: String
  }
  | GeneratedMapping
  { dstTpl :: String
  , indexSpec :: IndexSpec Unresolved
  , mappingOrigin :: String
  }

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space hspace1 empty empty

keyword :: Text -> Parser Text
keyword name = try (string name <* notFollowedBy alphaNumChar) <* sc

param :: Text -> Parser a -> Parser a
param name parser = string (name <> "=") *> parser <* sc

optionalParam :: Text -> Parser a -> Permutation Parser (Maybe a)
optionalParam name parser =
  toPermutationWithDefault Nothing (Just <$> param name parser)

requiredParam :: Text -> Parser a -> Permutation Parser a
requiredParam name = toPermutation . param name

someString :: Parser String
someString = (char '"' *> manyTill L.charLiteral (char '"'))
  <|> manyTill L.charLiteral (lookAhead (void spaceChar <|> eof))

someBool :: Parser Bool
someBool = (string "true" $> True) <|> (string "false" $> False)

someFile :: Parser (SomeBase File)
someFile = do
  raw <- someString
  case parseSomeFile raw of
    Nothing -> fail ("Invalid template file path: " ++ raw)
    Just path -> pure path

pIndexSpec :: Parser (IndexSpec Unresolved)
pIndexSpec =
  keyword "index" *> runPermutation
    ( IndexSpec
        <$> requiredParam "template" someFile
        <*> requiredParam "selection" someSelection
    )
 where
  someSelection = do
    start <- getOffset
    raw <- takeWhile1P (Just "selection regex") (not . isSpace)
    maybe (setOffset start *> fail "Invalid selection regex") pure $ compileSelection (T.unpack raw)

pAction :: Parser (Action Unresolved)
pAction =
  choice
    [ keyword "typst"
        *> runPermutation
          ( Typst
              <$> optionalParam "template" someFile
              <*> optionalParam "bodyOnly" someBool
          )
    , keyword "typstPDF" $> TypstPDF
    , keyword "copy" $> Copy
    , keyword "compose" $> Compose
    , Index <$> pIndexSpec
    ]

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

pRegexAt :: Int -> String -> Parser Regex
pRegexAt start src =
  maybe (setOffset start *> fail "Invalid regex pattern") pure $ makeRegexM ("\\`" ++ src ++ "\\'")

pMapping :: Parser Mapping
pMapping = (generated *> pGeneratedMapping) <|> pFileMapping
 where
  generated = try $ lookAhead $ keyword "index" *> (string "template=" <|> string "selection=")

pGeneratedMapping :: Parser Mapping
pGeneratedMapping = do
  origin <- sourcePosPretty <$> getSourcePos
  spec <- pIndexSpec
  dst <- pDestination False
  return $ GeneratedMapping dst spec origin

pDestination :: Bool -> Parser String
pDestination nested = do
  sc
  void $ string "=>"
  sc
  dst <- trim <$> manyTill anySingle (lookAhead end)
  when (null dst) $ fail "dst cannot be empty"
  return dst
 where
  end = if nested then void (oneOf [';', '}']) <|> void eol <|> eof else void eol <|> eof

blockSpace :: Parser ()
blockSpace = L.space space1 (L.skipLineComment "#") empty

pRoutedAction :: Bool -> Parser (Action Unresolved, String)
pRoutedAction nested = pChoice <|> pIgnore <|> ((,) <$> pAction <*> pDestination nested)
 where
  pIgnore = keyword "ignore" *> ((,) Ignore <$> option "" (pDestination nested))
  pChoice = do
    mode <- (keyword "choose" $> Selector) <|> (keyword "check" $> Predicate)
    script <- param "script" someFile
    blockSpace
    void $ char '{'
    blockSpace
    branches <- pBranch `sepEndBy1` (char ';' *> blockSpace)
    void $ char '}'
    let names = map fst branches
    when (length names /= length (nub names)) $ fail "Duplicate branch name"
    when (mode == Predicate && sort names /= ["no", "yes"]) $
      fail "A check requires exactly the yes and no branches"
    pure (Choose mode script branches, "")
  pBranch = do
    name <- T.pack <$> some (alphaNumChar <|> oneOf ['_', '-'])
    sc
    void $ char ':'
    blockSpace
    branch <- pRoutedAction True
    blockSpace
    pure (name, branch)

pFileMapping :: Parser Mapping
pFileMapping = do
  origin <- sourcePosPretty <$> getSourcePos
  start <- getOffset
  src <- trim <$> manyTill anySingle (try (string "=>"))
  when (null src) $ fail "src cannot be empty"
  pattern <- pRegexAt start src
  sc
  (parsedAction, dst) <- pRoutedAction False
  return $ Mapping pattern dst parsedAction origin

pMappings :: Parser [Mapping]
pMappings =
  catMaybes <$> many (notFollowedBy eof *> pLine) <* eof
 where
  pLine = do
    sc
    result <- optional (notFollowedBy (char '#') *> pMapping)
    sc
    void $ optional (L.skipLineComment "#")
    void eol <|> eof
    pure result

parseMappings :: Text -> String -> Either String [Mapping]
parseMappings input srcFile =
  case runParser pMappings srcFile input of
    Left err -> Left (errorBundlePretty err)
    Right mappings -> Right mappings
