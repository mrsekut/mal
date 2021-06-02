module Mal.Reader where

import Prelude
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.List (List(..), concat, many, (:))
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (endBy, skipMany, skipMany1, try)
import Text.Parsing.Parser.String (char, noneOf, oneOf)
import Text.Parsing.Parser.Token (digit, letter)
import Types (MalExpr(..))

spaces :: Parser String Unit
spaces = skipMany1 $ oneOf' ", \n"

comment :: Parser String Unit
comment = char ';' *> (skipMany $ noneOf [ '\r', '\n' ])

ignored :: Parser String Unit
ignored = skipMany $ spaces <|> comment

symbol :: Parser String Char
symbol = oneOf' "!#$%&|*+-/:<=>?@^_~"

readNumber :: Parser String MalExpr
readNumber = MalInt <$> nat

readNegativeNumber :: Parser String MalExpr
readNegativeNumber = MalInt <<< negate <$> (char '-' *> nat)

nat :: Parser String Int
nat = do
  first <- digit
  rest <- many digit
  pure <<< fromMaybe 0 <<< fromString <<< charListToString $ first : rest

escape :: Parser String (List Char)
escape = do
  b <- char '\\'
  c <- oneOf [ '\"', '\\' ]
  pure $ b : c : Nil

nonEscape :: Parser String (List Char)
nonEscape = do
  n <- noneOf [ '\"', '\\' ]
  pure $ n : Nil

readString :: Parser String MalExpr
readString =
  MalString
    <<< charListToString
    <$> (char '"' *> (concat <$> many (escape <|> nonEscape)) <* char '"')

-- FIXME: name
readSymbol :: Parser String MalExpr
readSymbol = f <$> (letter <|> symbol) <*> many (letter <|> digit <|> symbol)
  where
  f first rest = g $ charListToString (first : rest)

  g "true" = MalBoolean true

  g "false" = MalBoolean false

  g "nil" = MalNil

  g s = MalSymbol s

-- FIXME: ?
readKeyword :: Parser String MalExpr
readKeyword =
  MalString
    <$> charListToString
    <$> ((:) '\x029')
    <$> (char ':' *> many (letter <|> digit <|> symbol))

readAtom :: Parser String MalExpr
readAtom =
  readNumber
    <|> try readNegativeNumber
    <|> readString
    -- <|> readKeyword
    
    <|> readSymbol

-- FIXME: sepEndBy ?
readList :: Parser String MalExpr
readList = fix $ \_ -> MalList <$> (char '(' *> ignored *> (endBy readForm ignored) <* char ')')

-- readHashMap :: Parser String MalExpr
-- readHashMap = g >>> keyValuePairs =<< (char '{' *> ignored *> sepEndBy readForm ignored <* char '}')
--   where
--   g (Just pairs) = return $ MalHashMap (MetaData Nil) (Map.fromList pairs)
--   g Nothing = fail "invalid contents inside map braces"
-- addPrefix :: String -> MalExpr -> MalExpr
-- addPrefix s x = toList $ MalSymbol s : x : Nil
-- readQuote :: Parser String MalExpr
-- readQuote = addPrefix "quote" <$> (char '\'' *> readForm)
-- readQuasiquote :: Parser String MalExpr
-- readQuasiquote = addPrefix "quasiquote" <$> (char '`' *> readForm)
-- readSpliceUnquote :: Parser String MalExpr
-- readSpliceUnquote = addPrefix "splice-unquote" <$> (string "~@" *> readForm)
-- readUnquote :: Parser String MalExpr
-- readUnquote = addPrefix "unquote" <$> (char '~' *> readForm)
-- readDeref :: Parser String MalExpr
-- readDeref = addPrefix "deref" <$> (char '@' *> readForm)
-- readWithMeta :: Parser String MalExpr
-- readWithMeta = f <$> (char '^' *> readForm) <*> readForm
--   where
--   f m x = toList $ MalSymbol "with-meta" : x : m : Nil
-- read_macro :: Parser String MalExpr
-- read_macro = readQuote <|> readQuasiquote <|> try readSpliceUnquote <|> readUnquote <|> readDeref <|> readWithMeta
readVector :: Parser String MalExpr
readVector = fix $ \_ -> MalVector <$> (char '[' *> ignored *> endBy readForm ignored <* char ']')

readForm :: Parser String MalExpr
readForm = fix $ \_ -> ignored *> (readList <|> readVector <|> readAtom)

-- FIXME: 何もやってない感がすごい
readStr :: String -> Either String MalExpr
readStr str = case runParser str readForm of
  Left err -> Left $ show err
  Right val -> Right val

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------
-- DEPRECATED:
oneOf' :: String -> Parser String Char
oneOf' = oneOf <<< toCharArray

-- DEPRECATED:
noneOf' :: String -> Parser String Char
noneOf' = noneOf <<< toCharArray

charListToString :: List Char -> String
charListToString = fromCharArray <<< fromFoldable
