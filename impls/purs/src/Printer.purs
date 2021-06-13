module Printer where

import Prelude

import Data.List (List(..), (:))
import Data.Map (toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Types (Key(..), MalExpr(..), flatTuples, flatStrings, stringToCharList)
import Data.String.CodeUnits(singleton)



-- PRINT STRING

printStr :: MalExpr -> String
printStr MalNil           = "nil"
printStr (MalBoolean b)   = show b
printStr (MalInt n)       = show n
printStr (MalString str)  = "\"" <> (str # stringToCharList # map unescape # flatStrings) <> "\""
printStr (MalKeyword key) = key
printStr (MalSymbol name) = name
printStr (MalList xs)     = "(" <> printList xs <> ")"
printStr (MalVector vs)   = "[" <> printList vs <> "]"
printStr (MalHashMap hm)  = "{" <> (hm # toUnfoldable # flatTuples # printList) <> "}"
printStr (MalFunction _)  = "#<function>"


printList :: List MalExpr -> String
printList Nil     = ""
printList (x:Nil) = printStr x
printList (x:xs)  = printStr x <> " " <> printList xs



-- PRINT STRING READABLY

printStrReadably :: MalExpr -> String
printStrReadably (MalString str)  = str
printStrReadably (MalList xs)     = "(" <> printListReadably " " xs <> ")"
printStrReadably (MalVector vs)   = "[" <> printListReadably " " vs <> "]"
printStrReadably (MalHashMap hm)  = "{" <> (hm # toUnfoldable # flatTuples # printListReadably " ") <> "}"
printStrReadably ex               = printStr ex


printListReadably :: String -> List MalExpr ->  String
printListReadably _ Nil      = ""
printListReadably _ (x:Nil)  = printStrReadably x
printListReadably sep (x:xs) = printStrReadably x <> sep <> printListReadably sep xs



-- UTILS

unescape :: Char -> String
unescape '\n' = "\\n"
unescape '\\' = "\\\\"
unescape '"'  = "\\\""
unescape c    = singleton c


keyValuePairs :: List MalExpr -> Maybe (List (Tuple Key MalExpr))
keyValuePairs Nil                      = pure Nil
keyValuePairs (MalString k : v : kvs)  = (:) (Tuple (StringKey k) v) <$> keyValuePairs kvs
keyValuePairs (MalKeyword k : v : kvs) = (:) (Tuple (KeywordKey k) v) <$> keyValuePairs kvs
keyValuePairs _                        = Nothing
