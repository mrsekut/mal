module Types where

import Data.List (List)
import Data.Map (Map)
import Data.Map.Internal (fromFoldable)
import Data.Tuple (Tuple)
import Data.Ord (class Ord)
import Data.Eq (class Eq)

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


data Key = StringKey String | KeywordKey String

derive instance eqKey :: Eq Key
derive instance ordKey :: Ord Key

-- instance ordKey :: Ord Key  where
--   show true = "true"
--   show false = "false"

-- | MalFunction MalFunction
-- | MalApply ApplyRec
-- | MalAtom Int
--


-- |
-- | Utils
-- |
toList :: List MalExpr -> MalExpr
toList = MalList

listToMap :: List (Tuple Key MalExpr) -> Map Key MalExpr
listToMap = fromFoldable
