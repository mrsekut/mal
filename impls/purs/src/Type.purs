module Types where

import Prelude

import Data.Array as Array
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map.Internal as Map
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))


data MalExpr
  = MalNil
  | MalBoolean Boolean
  | MalInt Int
  | MalString String
  | MalKeyword String
  | MalSymbol String
  | MalList (List MalExpr)
  | MalVector (List MalExpr)
  | MalHashMap (Map Key MalExpr)


data Key = StringKey String
         | KeywordKey String
derive instance eqKey :: Eq Key
derive instance ordKey :: Ord Key




----------------------------------------------------------------
-- Utils
----------------------------------------------------------------


toList :: List MalExpr -> MalExpr
toList = MalList

listToMap :: List (Tuple Key MalExpr) -> Map Key MalExpr
listToMap = Map.fromFoldable

charListToString :: List Char -> String
charListToString = fromCharArray <<< Array.fromFoldable

flatTuples :: List (Tuple Key MalExpr) -> List MalExpr
flatTuples ((Tuple (StringKey a) b) : xs)   = MalString a : b : flatTuples xs
flatTuples ((Tuple (KeywordKey a) b) : xs)  = MalKeyword a : b : flatTuples xs
flatTuples _                                = Nil
