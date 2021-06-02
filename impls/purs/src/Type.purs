module Types where

import Data.List (List)

-- import Data.Map (Map)
data MalExpr
  = MalNil
  | MalBoolean Boolean
  | MalInt Int
  | MalString String
  -- | MalKeyword String
  | MalSymbol String
  | MalList (List MalExpr)
  | MalVector (List MalExpr)
  -- FIXME: keyが重複しうるのでやっぱりMap, Tupleじゃないとだめだな
  | MalHashMap (List { key :: String, val :: MalExpr })

-- | MalFunction MalFunction
-- | MalApply ApplyRec
-- | MalAtom Int
toList :: List MalExpr -> MalExpr
toList = MalList
