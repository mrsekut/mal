module Printer where

import Prelude
import Data.Array (intersperse)
import Data.List (List(..), concat, concatMap, fold, foldr, fromFoldable, (:))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Effect.Aff (Aff)
import Types (MalExpr(..))

-- import qualified Data.Map as Map
-- import Data.IORef (readIORef)
-- import Types
printList :: List MalExpr -> String
printList Nil = ""

printList (x : Nil) = printStr x

printList (x : xs) = printStr x <> " " <> printList xs

-- _flatTuples :: [(String, MalVal)] -> [MalVal]
-- _flatTuples ((a,b):xs) = MalString a : b : _flatTuples xs
-- _flatTuples _          = []
-- unescape :: Char -> String
-- unescape '\n' = "\\n"
-- unescape '\\' = "\\\\"
-- unescape '"' = "\\\""
-- unescape c = show c
-- stringToCharList :: String -> List Char
-- stringToCharList = fromFoldable <<< toCharArray
-- b str = foldr (<>) "" $ map unescape (stringToCharList str)
oo :: List { key :: String, val :: MalExpr } -> String
oo ls = foldr (<>) "" $ map f ls

f :: { key :: String, val :: MalExpr } -> String
f s = "\"" <> s.key <> "\"" <> " " <> printStr s.val

printStr :: MalExpr -> String
printStr (MalString str) = "\"" <> str <> "\""

-- printStr (MalString str) = "\"" <> b str <> "\""
printStr (MalInt num) = show num

printStr (MalBoolean true) = "true"

printStr (MalBoolean false) = "false"

printStr MalNil = "nil"

printStr (MalSymbol name) = name

printStr (MalList items) = "(" <> printList items <> ")"

printStr (MalVector items) = "[" <> printList items <> "]"

printStr (MalHashMap ls) = format $ oo ls
  where
  format x = "{" <> x <> "}"

-- printStr (MalHashMap _ m) = format <$> printList " " (_flatTuples $ assocs m)
--   where
--   format x = "{" <> x <> "}"
-- printStr pr (MalAtom _ r) = format  <$> (printStr pr =<< readIORef r) where
--     format x = "(atom " ++ x ++ ")"
-- printStr _ (MalFunction {f_ast=Nil}) = pure "#<function>"
-- printStr _ (MalFunction {f_ast=a, f_params=p, macro=False}) = format <$> printStr True a where
--     format x = "(fn* " ++ show p ++ " -> " ++ x ++ ")"
-- printStr _ (MalFunction {f_ast=a, f_params=p, macro=True})  = format <$> printStr True a where
--     format x = "(macro* " ++ show p ++ " -> " ++ x ++ ")"
keyValuePairs :: List MalExpr -> Maybe (List { key :: String, val :: MalExpr })
keyValuePairs Nil = pure Nil

keyValuePairs (MalString k : v : kvs) = do
  let
    a = { key: k, val: v }
  b <- keyValuePairs kvs
  pure $ a : b

keyValuePairs _ = Nothing
