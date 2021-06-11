module Core where

import Prelude

import Data.List (List(..), fromFoldable, (:), length)
import Data.Tuple (Tuple(..))
import Env (throwStr)
import Types (MalExpr(..), MalFn, toList)




ns :: List (Tuple String MalFn)
ns = fromFoldable
  [ Tuple "="           eqQ
  , Tuple "+"           $ numOp (+)
  , Tuple "-"           $ numOp (-)
  , Tuple "*"           $ numOp (*)
  , Tuple "/"           $ numOp (/)
  , Tuple "<"           $ cmpOp (<)
  , Tuple "<="          $ cmpOp (<=)
  , Tuple ">"           $ cmpOp (>)
  , Tuple ">="          $ cmpOp (>=)

  , Tuple "list"        list
  , Tuple "list?"       $ fn1 listQ
  , Tuple "nil?"        $ fn1 nilQ
  , Tuple "empty?"      $ fn1 emptyQ
  , Tuple "count"       count
  ]



--

-- FIXME: name
fn1 :: (MalExpr -> Boolean) -> MalFn
fn1 f (x:Nil) = pure $ MalBoolean $ f x
fn1 _ _       = throwStr "illegal call to unary predicate"


numOp :: (Int -> Int -> Int) -> MalFn
numOp op ((MalInt n1) : (MalInt n2) : Nil) = pure $ MalInt $ op n1 n2
numOp _ _                                  = throwStr "invalid operator"


cmpOp :: (Int -> Int -> Boolean) -> MalFn
cmpOp op ((MalInt n1) : (MalInt n2) : Nil) = pure $ MalBoolean $ op n1 n2
cmpOp _ _                                  = throwStr "invalid operator"


list :: MalFn
list = pure <<< toList


nilQ :: MalExpr -> Boolean
nilQ MalNil = true
nilQ _      = false


listQ :: MalExpr -> Boolean
listQ (MalList _) = true
listQ _           = false


emptyQ :: MalExpr -> Boolean
emptyQ (MalList Nil) = true
emptyQ _             = false


count :: MalFn
count (MalNil : Nil)      = pure $ MalInt 0
count (MalList Nil : Nil) = pure $ MalInt 0
count (MalList ex : Nil)  = pure $ MalInt $ length ex
count _                   = throwStr "non-sequence passed to count"


eqQ :: MalFn
eqQ ( a :b : Nil) = pure $ MalBoolean $ a == b
eqQ _ = throwStr "illegal arguments to ="
