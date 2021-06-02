module Mal.Reader where

import Prelude
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.List (List(..), concat, many, (:))
import Data.Map (union)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Printer (keyValuePairs)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (endBy, sepEndBy, skipMany, skipMany1, try)
import Text.Parsing.Parser.String (char, noneOf, oneOf, string)
import Text.Parsing.Parser.Token (digit, letter)
import Types (MalExpr(..), toList)

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
  c <- oneOf [ '\"', '\\', 'n' ]
  pure $ b : c : Nil

nonEscape :: Parser String (List Char)
nonEscape = do
  n <- noneOf [ '\"', '\\' ]
  pure $ n : Nil

-- escaped :: Parser String Char
-- escaped = f <$> (char '\\' *> oneOf [ '\\', '\"', 'n' ])
--   where
--   f 'n' = '\n'
--   f x = x
readString :: Parser String MalExpr
readString =
  MalString
    <<< charListToString
    <$> (char '"' *> (concat <$> many (escape <|> nonEscape)) <* char '"')

-- readString =
--   MalString
--     <<< charListToString
--     <$> (char '"' *> many (escaped <|> noneOf [ '\\', '\"' ]) <* char '"')
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

mmm :: Parser String (List MalExpr)
mmm = fix $ \_ -> char '{' *> ignored *> endBy readForm ignored <* char '}'

readHashMap :: Parser String MalExpr
readHashMap = do
  m <- mmm
  pure $ g $ keyValuePairs m
  where
  g :: Maybe (List { key :: String, val :: MalExpr }) -> MalExpr
  g (Just pairs) = MalHashMap pairs

  g Nothing = MalString "hash map error" -- FIXME: error

addPrefix :: String -> MalExpr -> MalExpr
addPrefix s x = toList $ MalSymbol s : x : Nil

readQuote :: Parser String MalExpr
readQuote = fix $ \_ -> addPrefix "quote" <$> (char '\'' *> readForm)

readQuasiquote :: Parser String MalExpr
readQuasiquote = fix $ \_ -> addPrefix "quasiquote" <$> (char '`' *> readForm)

readSpliceUnquote :: Parser String MalExpr
readSpliceUnquote = fix $ \_ -> addPrefix "splice-unquote" <$> (string "~@" *> readForm)

readUnquote :: Parser String MalExpr
readUnquote = fix $ \_ -> addPrefix "unquote" <$> (char '~' *> readForm)

readDeref :: Parser String MalExpr
readDeref = fix $ \_ -> addPrefix "deref" <$> (char '@' *> readForm)

-- readWithMeta :: Parser String MalExpr
-- readWithMeta = f <$> (char '^' *> readForm) <*> readForm
--   where
--   f m x = toList $ MalSymbol "with-meta" : x : m : Nil
readMacro :: Parser String MalExpr
readMacro =
  fix
    $ \_ ->
        readQuote
          <|> readQuasiquote
          <|> try readSpliceUnquote
          <|> readUnquote
          <|> readDeref

-- read_macro = readQuote <|> readQuasiquote <|> try readSpliceUnquote <|> readUnquote <|> readDeref <|> readWithMeta
readVector :: Parser String MalExpr
readVector = fix $ \_ -> MalVector <$> (char '[' *> ignored *> endBy readForm ignored <* char ']')

readForm :: Parser String MalExpr
readForm =
  fix
    $ \_ ->
        ignored
          *> ( readMacro
                <|> readList
                <|> readVector
                <|> readHashMap
                <|> readAtom
            )

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

-- FIXME: move
charListToString :: List Char -> String
charListToString = fromCharArray <<< fromFoldable
