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
import Query (Query (..), defaultQuery, parsePredicate, validateField)

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

someSelection :: Bool -> Parser Selection
someSelection nested = do
  start <- getOffset
  raw <- (T.pack <$> (char '"' *> manyTill L.charLiteral (char '"')))
    <|> takeWhile1P (Just "selection regex") (\c -> not (isSpace c) && (not nested || c /= ';'))
  maybe (setOffset start *> fail "Invalid selection regex") pure $ compileSelection (T.unpack raw)

pIndexSpec :: Parser (IndexSpec Unresolved)
pIndexSpec = do
  void $ keyword "index"
  (indexFile, legacy) <- runPermutation $ (,)
    <$> requiredParam "template" someFile
    <*> optionalParam "selection" (someSelection False)
  named <- optional $ do
    void $ try $ blockSpace *> char '{'
    blockSpace
    selections <- namedSelection `sepEndBy1` (char ';' *> blockSpace)
    void $ char '}'
    let names = map fst selections
    when (length names /= length (nub names)) $ fail "Duplicate index selection name"
    when (any (`elem` ["meta", "child", "title"]) names) $ fail "Reserved index selection name"
    pure selections
  selections <- case (legacy, named) of
    (Just selected, Nothing) -> pure [("entries", IndexSelection selected defaultQuery)]
    (Nothing, Just selections) -> pure selections
    _ -> fail "An index requires either selection=... or a named selection block"
  pure $ IndexSpec indexFile selections
 where
  namedSelection = do
    name <- T.pack <$> some (alphaNumChar <|> oneOf ['_', '-'])
    sc
    void $ char ':'
    blockSpace
    let parameter key parser = param key parser <* blockSpace
        required key = toPermutation . parameter key
        optionalValue key parser = toPermutationWithDefault Nothing (Just <$> parameter key parser)
        sortField = do
          field <- takeWhile1P (Just "sort field") (\c -> not (isSpace c) && c `notElem` [';', '}'])
          either fail pure $ validateField field
        direction = (keyword "asc" $> False) <|> (keyword "desc" $> True)
        dateFormat = do
          value <- (lookAhead (char '\"') *> someString)
            <|> some (satisfy (\c -> not (isSpace c) && c `notElem` [';', '}']))
          when (null value) $ fail "dateFormat cannot be empty"
          pure value
        limit = do
          n <- L.decimal :: Parser Integer
          when (n > toInteger (maxBound :: Int)) $ fail "Index limit is too large"
          pure $ fromInteger n
    (selected, predicate, field, descending, entryLimit, format) <- runPermutation $ (,,,,,)
      <$> required "selection" (someSelection True)
      <*> optionalValue "filter" (someString >>= either fail pure . parsePredicate)
      <*> optionalValue "sort" sortField
      <*> optionalValue "order" direction
      <*> optionalValue "limit" limit
      <*> optionalValue "dateFormat" dateFormat
    when (descending /= Nothing && field == Nothing) $ fail "order requires sort"
    when (format /= Nothing && field == Nothing) $ fail "dateFormat requires sort"
    pure (name, IndexSelection selected $ Query predicate field (descending == Just True) entryLimit format)

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
