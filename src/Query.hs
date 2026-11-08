{-# LANGUAGE OverloadedStrings #-}

module Query (Query (..), Predicate, defaultQuery, parsePredicate, validateField, applyQuery) where

import Control.Monad (unless)
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Query = Query
  { queryFilter :: Maybe Predicate
  , querySort :: Maybe Text
  , queryDescending :: Bool
  , queryLimit :: Maybe Int
  }
  deriving (Show, Eq)

data Operand = Field Text | Literal Value deriving (Show, Eq)
data Predicate = Equal Operand Operand | NotEqual Operand Operand | Member Operand Operand deriving (Show, Eq)

defaultQuery :: Query
defaultQuery = Query Nothing Nothing False Nothing

type Parser = Parsec Void Text

validateField :: Text -> Either String Text
validateField field
  | field `elem` ["title", "source", "target", "url"] = Right field
  | "meta." `T.isPrefixOf` field && all (not . T.null) (T.splitOn "." field) = Right field
  | otherwise = Left $ "Unknown entry field " ++ show field ++ "; use meta.<key> for metadata."

parsePredicate :: String -> Either String Predicate
parsePredicate = either (Left . errorBundlePretty) Right . runParser (space *> predicate <* eof) "filter" . T.pack
 where
  lexeme :: Parser a -> Parser a
  lexeme = L.lexeme space
  word value = lexeme $ try $ string value <* notFollowedBy (alphaNumChar <|> char '_')
  stringValue quote = char quote *> manyTill L.charLiteral (char quote)
  field = T.intercalate "." <$> (T.pack <$> ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> oneOf ['_', '-']))) `sepBy1` char '.'
  operand = choice
    [ Literal . String . T.pack <$> lexeme (stringValue '\'' <|> stringValue '"')
    , word "true" *> pure (Literal $ Bool True)
    , word "false" *> pure (Literal $ Bool False)
    , word "null" *> pure (Literal Null)
    , Literal . Number <$> lexeme (L.signed (pure ()) L.scientific)
    , Field <$> (lexeme field >>= either fail pure . validateField)
    ]
  predicate = do
    left <- operand
    op <- (lexeme (string "==") *> pure Equal)
      <|> (lexeme (string "!=") *> pure NotEqual)
      <|> (word "in" *> pure Member)
    op left <$> operand

lookupField :: Text -> Value -> Value
lookupField field = descend $ T.splitOn "." field
 where
  descend [] value = value
  descend (key : rest) (Object fields) = descend rest $ fromMaybe Null $ KM.lookup (K.fromText key) fields
  descend _ _ = Null

matches :: Value -> Predicate -> Bool
matches entry predicate = case predicate of
  Equal a b -> value a == value b
  NotEqual a b -> value a /= value b
  Member a b -> case value b of
    Array values -> value a `elem` toList values
    _ -> False
 where
  value (Literal literal) = literal
  value (Field field) = lookupField field entry

applyQuery :: Query -> [Value] -> Either String [Value]
applyQuery query entries = do
  let filtered = filter (\entry -> maybe True (matches entry) $ queryFilter query) entries
  ordered <- case querySort query of
    Nothing -> pure filtered
    Just field -> do
      let keyed = [(lookupField field entry, entry) | entry <- filtered]
          kinds = [kind key | (key, _) <- keyed, key /= Null]
      unless (all (/= 0) kinds && (null kinds || all (== head kinds) kinds)) $
        Left $ "Sort field " ++ show field ++ " must contain one scalar type (strings, numbers, or booleans)."
      pure $ map snd $ sortBy compareEntry keyed
  pure $ maybe ordered (`take` ordered) $ queryLimit query
 where
  kind (String _) = 1 :: Int
  kind (Number _) = 2
  kind (Bool _) = 3
  kind _ = 0
  compareEntry (a, x) (b, y) = case compareKey a b of
    EQ -> compare (lookupField "source" x) (lookupField "source" y)
    order -> order
  compareKey Null Null = EQ
  compareKey Null _ = GT
  compareKey _ Null = LT
  compareKey a b = if queryDescending query then compare b a else compare a b
