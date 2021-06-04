module Printer where

import Prelude

import Data.List (List(..), (:))
import Data.Map (toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Types (Key(..), MalExpr(..), flatTuples)


printStr :: MalExpr -> String
printStr (MalString str) = "\"" <> str <> "\""
printStr (MalInt num)       = show num
printStr (MalKeyword k)     = k
printStr (MalBoolean true)  = "true"
printStr (MalBoolean false) = "false"
printStr MalNil             = "nil"
printStr (MalSymbol name)   = name
printStr (MalList Nil)      = "(" <> ")"
printStr (MalList items)    = "(" <> printList items <> ")"
printStr (MalVector Nil)    = "[" <> "]"
printStr (MalVector items)  = "[" <> printList items <> "]"
printStr (MalHashMap ms)    = "{" <> (ms # toUnfoldable # flatTuples # printList) <> "}"
printStr (MalFunction _)    = "#<function>"

printList :: List MalExpr -> String
printList Nil       = ""
printList (x : Nil) = printStr x
printList (x : xs)  = printStr x <> " " <> printList xs

keyValuePairs :: List MalExpr -> Maybe (List (Tuple Key MalExpr))
keyValuePairs Nil                       = pure Nil
keyValuePairs (MalString k : v : kvs)   = (:) (Tuple (StringKey k) v) <$> keyValuePairs kvs
keyValuePairs (MalKeyword k : v : kvs)  = (:) (Tuple (KeywordKey k) v) <$> keyValuePairs kvs
keyValuePairs _                         = Nothing
