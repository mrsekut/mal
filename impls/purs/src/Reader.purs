module Mal.Reader where

import Prelude
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (fromFoldable, toUnfoldable)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.List (List(..), many, some, (:))
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (sepBy, sepEndBy, skipMany, skipMany1, try)
import Text.Parsing.Parser.String (char, noneOf, oneOf, string)
import Text.Parsing.Parser.Token (digit, letter)
import Types (MalExpr(..), toList)

-- FIXME: name
type P a
  = Parser String a

oneOf' :: String -> P Char
oneOf' = oneOf <<< toCharArray

noneOf' :: String -> P Char
noneOf' = noneOf <<< toCharArray

spaces :: P Unit
spaces = skipMany1 $ oneOf' ", \n"

comment :: P Unit
comment = char ';' *> (skipMany $ noneOf' "\r\n")

ignored :: P Unit
ignored = skipMany (spaces <|> comment)

symbol :: P Char
symbol = oneOf' "!#$%&|*+-/:<=>?@^_~"

readNumber :: P MalExpr
readNumber = MalInt <$> nat

readNegativeNumber :: P MalExpr
readNegativeNumber = f <$> char '-' <*> nat
  where
  f sign rest = MalInt $ negate rest

nat :: P Int
nat = do
  first <- digit
  rest <- many digit
  pure <<< fromMaybe 0 <<< fromString <<< charListToString $ first : rest

charListToString :: List Char -> String
charListToString = fromCharArray <<< fromFoldable

-- FIXME: name
read_string :: P MalExpr
read_string = do
  rest <- char '"' *> (many $ letter <|> symbol <|> digit <|> char ' ') <* char '"'
  pure <<< MalString <<< charListToString $ rest

readSymbol :: P MalExpr
readSymbol = f <$> (letter <|> symbol) <*> many (letter <|> digit <|> symbol)
  where
  f first rest = g $ charListToString (first : rest)

  g "true" = MalBoolean true

  g "false" = MalBoolean false

  g "nil" = MalNil

  g s = MalSymbol s

readKeyword :: P MalExpr
readKeyword = MalString <$> charListToString <$> ((:) '\x029') <$> (char ':' *> many (letter <|> digit <|> symbol))

readAtom :: P MalExpr
readAtom = readNumber <|> try readNegativeNumber <|> read_string <|> readKeyword <|> readSymbol

readList :: P MalExpr
readList = fix $ \x -> MalList <$> (char '(' *> ignored *> (sepEndBy readForm ignored) <* char ')')

-- readHashMap :: P MalExpr
-- readHashMap = g >>> keyValuePairs =<< (char '{' *> ignored *> sepEndBy readForm ignored <* char '}')
--   where
--   g (Just pairs) = return $ MalHashMap (MetaData Nil) (Map.fromList pairs)
--   g Nothing = fail "invalid contents inside map braces"
addPrefix :: String -> MalExpr -> MalExpr
addPrefix s x = toList $ MalSymbol s : x : Nil

readQuote :: P MalExpr
readQuote = addPrefix "quote" <$> (char '\'' *> readForm)

readQuasiquote :: P MalExpr
readQuasiquote = addPrefix "quasiquote" <$> (char '`' *> readForm)

readSpliceUnquote :: P MalExpr
readSpliceUnquote = addPrefix "splice-unquote" <$> (string "~@" *> readForm)

readUnquote :: P MalExpr
readUnquote = addPrefix "unquote" <$> (char '~' *> readForm)

readDeref :: P MalExpr
readDeref = addPrefix "deref" <$> (char '@' *> readForm)

readWithMeta :: P MalExpr
readWithMeta = f <$> (char '^' *> readForm) <*> readForm
  where
  f m x = toList $ MalSymbol "with-meta" : x : m : Nil

read_macro :: P MalExpr
read_macro = readQuote <|> readQuasiquote <|> try readSpliceUnquote <|> readUnquote <|> readDeref <|> readWithMeta

readVector :: P MalExpr
readVector = fix $ \x -> MalVector <$> (char '[' *> ignored *> sepEndBy readForm ignored <* char ']')

readForm :: P MalExpr
readForm = fix $ \s -> ignored *> (readList <|> readVector <|> readAtom)

-- FIXME: 何もやってない感がすごい
readString :: String -> Either String MalExpr
readString str = case runParser str readForm of
  Left err -> Left $ show err
  Right val -> Right val
