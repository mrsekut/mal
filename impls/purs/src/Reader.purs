module Mal.Reader where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.List (List(..), concat, many, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple)
import Printer (keyValuePairs)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (endBy, skipMany, skipMany1, try)
import Text.Parsing.Parser.String (char, noneOf, oneOf, string)
import Text.Parsing.Parser.Token (digit, letter)
import Types (Key, MalExpr(..), charListToString, listToMap, toList)


spaces :: Parser String Unit
spaces = skipMany1 $ oneOf [',', ' ', '\n']

comment :: Parser String Unit
comment = char ';' *> (skipMany $ noneOf [ '\r', '\n' ])

ignored :: Parser String Unit
ignored = skipMany $ spaces <|> comment

symbol :: Parser String Char
symbol = oneOf ['!', '#', '$', '%', '&', '|', '*', '+', '-', '/', ':', '<', '=', '>', '?', '@', '^', '_', '~']

nat :: Parser String Int
nat = do
  first <- digit
  rest <- many digit
  pure <<< fromMaybe 0 <<< fromString <<< charListToString $ first : rest

escape :: Parser String (List Char)
escape = do
  b <- char '\\'
  c <- oneOf [ '\"', '\\', 'n' ]
  pure $ b : c : Nil

nonEscape :: Parser String (List Char)
nonEscape = do
  n <- noneOf [ '\"', '\\' ]
  pure $ n : Nil


----------------------------------------------------------------
-- reader atom
----------------------------------------------------------------

readNumber :: Parser String MalExpr
readNumber = MalInt <$> nat

readNegativeNumber :: Parser String MalExpr
readNegativeNumber = MalInt <<< negate <$> (char '-' *> nat)

readString :: Parser String MalExpr
readString = MalString
         <<< charListToString
         <$> (char '"' *> (concat <$> many (escape <|> nonEscape)) <* char '"')

readKeyword :: Parser String MalExpr
readKeyword =
  MalKeyword <$> charListToString
             <$> ((:) ':')
             <$> (char ':' *> many (letter <|> digit <|> symbol))

readSymbol :: Parser String MalExpr
readSymbol = f <$> (letter <|> symbol) <*> many (letter <|> digit <|> symbol)
  where
  f first rest = g $ charListToString (first : rest)
  g "true"  = MalBoolean true
  g "false" = MalBoolean false
  g "nil"   = MalNil
  g s       = MalSymbol s

readAtom :: Parser String MalExpr
readAtom = readNumber
       <|> try readNegativeNumber
       <|> readString
       <|> readKeyword
       <|> readSymbol



----------------------------------------------------------------

readList :: Parser String MalExpr
readList = fix $ \_ ->
  MalList <$> (char '(' *> ignored *> endBy readForm ignored <* char ')')



----------------------------------------------------------------

readVector :: Parser String MalExpr
readVector = fix $ \_ ->
  MalVector <$> (char '[' *> ignored *> endBy readForm ignored <* char ']')



----------------------------------------------------------------

readHashMap :: Parser String MalExpr
readHashMap = do
  es <- fix $ \_ -> char '{' *> ignored *> endBy readForm ignored <* char '}'
  pure $ g $ keyValuePairs es
  where
  g :: Maybe (List (Tuple Key MalExpr)) -> MalExpr
  g (Just ts) = MalHashMap $ listToMap ts
  g Nothing   = MalString "hash map error" -- FIXME: error



----------------------------------------------------------------
-- reader macros
----------------------------------------------------------------

macro :: String -> String -> Parser String MalExpr
macro tok sym = addPrefix sym <$> (string tok *> readForm)
  where
  addPrefix :: String -> MalExpr -> MalExpr
  addPrefix s x = toList $ MalSymbol s : x : Nil

readWithMeta :: Parser String MalExpr
readWithMeta = addPrefix <$> (char '^' *> readForm) <*> readForm
  where
  addPrefix :: MalExpr -> MalExpr -> MalExpr
  addPrefix m x = toList $ MalSymbol "with-meta" : x : m : Nil

readMacro :: Parser String MalExpr
readMacro =
  fix $ \_ -> macro "\'" "quote"
          <|> macro "`" "quasiquote"
          <|> try (macro "~@" "splice-unquote")
          <|> macro "~" "unquote"
          <|> macro "@" "deref"
          <|> readWithMeta



----------------------------------------------------------------

readForm :: Parser String MalExpr
readForm =
  fix $ \_ -> ignored
          *> ( readMacro
            <|> readList
            <|> readVector
            <|> readHashMap
            <|> readAtom
             )



----------------------------------------------------------------


-- FIXME: 何もやってない感がすごい
readStr :: String -> Either String MalExpr
readStr str = case runParser str readForm of
  Left err  -> Left $ show err
  Right val -> Right val
