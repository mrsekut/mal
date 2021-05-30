module Types where

import Data.List (List)

data MalExpr
  = MalNil
  | MalBoolean Boolean
  | MalInt Int
  | MalString String
  -- | MalKeyword String
  | MalSymbol String

-- | MalList (List MalExpr)
-- | MalVector (List MalExpr)
-- | MalMap (Dict String MalExpr)
-- | MalFunction MalFunction
-- | MalApply ApplyRec
-- | MalAtom Int
