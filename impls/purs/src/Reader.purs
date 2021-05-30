module Mal.Reader where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Error.Class (try)
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.List (List, many, (:))
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (sepBy, sepEndBy, skipMany, skipMany1)
import Text.Parsing.Parser.String (char, noneOf, oneOf)
import Text.Parsing.Parser.Token (digit, letter)
import Types (MalExpr(..))

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

read_number :: P MalExpr
read_number = do
  f <- sign
  n <- nat
  pure $ MalInt $ f n

sign :: ∀ a. (Ring a) => P (a -> a)
sign =
  (char '-' $> negate)
    <|> pure identity

nat :: P Int
nat = do
  first <- digit
  rest <- many digit
  pure <<< fromMaybe 0 <<< fromString <<< charListToString $ first : rest

charListToString :: List Char -> String
charListToString = fromCharArray <<< fromFoldable

read_string :: P MalExpr
read_string = do
  rest <- char '"' *> (many $ letter <|> symbol <|> digit <|> char ' ') <* char '"'
  pure <<< MalString <<< charListToString $ rest

read_symbol :: P MalExpr
read_symbol = f <$> (letter <|> symbol) <*> many (letter <|> digit <|> symbol)
  where
  f first rest = g $ charListToString (first : rest)

  g "true" = MalBoolean true

  g "false" = MalBoolean false

  g "nil" = MalNil

  g s = MalSymbol s

-- read_keyword :: P MalExpr
-- read_keyword = MalKeyword $ (:) '\x029e' <$> a
-- a = (char ':' *> many (letter <|> digit <|> symbol))
readAtom :: P MalExpr
readAtom =
  read_number
    <|> read_string
    <|> read_symbol

-- read_list :: P MalExpr
-- read_list = MalList <$> (char '(' *> ignored *> sepEndBy readForm ignored <* char ')')
-- parseDottedList :: SParser Expr
-- parseDottedList = do
--   init <- many $ whiteSpace *> parseAtom <* whiteSpace
--   _ <- whiteSpace *> char '.' <* whiteSpace
--   rest <- whiteSpace *> parseAtom <* whiteSpace
--   pure $ DottedList init rest
-- read_vector :: P MalExpr
-- read_vector = MalVector <$> (char '[' *> ignored *> sepEndBy readForm ignored <* char ']')
readForm :: P MalExpr
readForm = ignored *> (readAtom)

-- FIXME: 何もやってない感がすごい
readString :: String -> Either String MalExpr
readString str = case runParser str readForm of
  Left err -> Left $ show err
  Right val -> Right val
