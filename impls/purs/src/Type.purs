module Types where

import Data.List (List)

data MalExpr
  = MalNil
  | MalBoolean Boolean
  | MalInt Int
  | MalString String
  -- | MalKeyword String
  | MalSymbol String
  | MalList (List MalExpr)
  | MalVector (List MalExpr)

-- | MalMap (Dict String MalExpr)
-- | MalFunction MalFunction
-- | MalApply ApplyRec
-- | MalAtom Int
toList :: List MalExpr -> MalExpr
toList = MalList

-- keyValuePairs :: List MalExpr -> Maybe List (String, MalExpr)
-- keyValuePairs []                      = pure []
-- keyValuePairs (MalString k : v : kvs) = ((k, v) :) <$> keyValuePairs kvs
-- keyValuePairs _ = Nothing
