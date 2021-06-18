module Core (ns) where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..), concat, fromFoldable, length, (:))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Mal.Reader (readStr)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Printer (printListReadably, printList)
import Types (MalExpr(..), MalFn)



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

  , Tuple "pr-str"      prStr
  , Tuple "str"         str
  , Tuple "prn"         prn
  , Tuple "println"     println
  , Tuple "slurp"       slurp
  , Tuple "read-string" readString

  , Tuple "list"        list
  , Tuple "list?"       $ pred1 listQ
  , Tuple "nil?"        $ pred1 nilQ
  , Tuple "empty?"      $ pred1 emptyQ
  , Tuple "count"       count
  , Tuple "cons"        cons
  , Tuple "concat"      concat'

  , Tuple "vec"         vec

  , Tuple "atom"        atom
  , Tuple "atom?"       $ pred1 atomQ
  , Tuple "deref"       deref
  , Tuple "reset!"      resetB
  , Tuple "swap!"       swapB
  ]



-- General functions

eqQ :: MalFn
eqQ (a:b:Nil) = pure $ MalBoolean $ a == b
eqQ _         = throw "illegal arguments to ="



-- Error/Exception functions



-- Scalar functions



-- Numeric functions

numOp :: (Int -> Int -> Int) -> MalFn
numOp op ((MalInt n1) : (MalInt n2) : Nil) = pure $ MalInt $ op n1 n2
numOp _ _                                  = throw "invalid operator"


cmpOp :: (Int -> Int -> Boolean) -> MalFn
cmpOp op ((MalInt n1) : (MalInt n2) : Nil) = pure $ MalBoolean $ op n1 n2
cmpOp _ _                                  = throw "invalid operator"



-- String functions

prStr :: MalFn
prStr a = liftEffect $ MalString <$> printList a
-- prStr = pure <<< MalString <<< printList


str :: MalFn
str a = liftEffect $ MalString <$> printListReadably "" a
-- str = pure <<< MalString <<< printListReadably ""


prn :: MalFn
prn args = liftEffect $ do
  log =<< printList args
  pure MalNil


println :: MalFn
println args = liftEffect $ do
  log =<< printListReadably " " args
  pure MalNil


slurp :: MalFn
slurp (MalString path : Nil) = MalString <$> liftEffect (readTextFile UTF8 path)
slurp _                      = throw "invalid arguments to slurp"


readString :: MalFn
readString (MalString s : Nil) = case readStr s of
  Left _    -> throw "invalid read-string"
  Right val ->  pure val
readString _                   = throw "invalid read-string"



-- List functions

list :: MalFn
list = pure <<< MalList


listQ :: MalExpr -> Boolean
listQ (MalList _) = true
listQ _           = false


nilQ :: MalExpr -> Boolean
nilQ MalNil = true
nilQ _      = false


emptyQ :: MalExpr -> Boolean
emptyQ (MalList Nil)   = true
emptyQ (MalVector Nil) = true
emptyQ _               = false


count :: MalFn
count (MalNil:Nil)         = pure $ MalInt 0
count (MalList ex : Nil)   = pure $ MalInt $ length ex
count (MalVector ex : Nil) = pure $ MalInt $ length ex
count _                    = throw "non-sequence passed to count"


cons :: MalFn
cons (x:Nil)                  = pure $ MalList $ x:Nil
cons (x : MalList xs : Nil)   = pure $ MalList $ x:xs
cons (x : MalVector xs : Nil) = pure $ MalList $ x:xs
cons _                        = throw "illegal call to cons"


concat' :: MalFn
concat' args = MalList <<< concat <$> traverse unwrapSeq args
  where

  unwrapSeq :: MalExpr -> Effect (List MalExpr)
  unwrapSeq (MalList xs)   = pure xs
  unwrapSeq (MalVector xs) = pure xs
  unwrapSeq _              = throw "invalid concat"



-- Vector functions

vec :: MalFn
vec (MalList xs : Nil)   = pure $ MalVector xs
vec (MalVector xs : Nil) = pure $ MalVector xs
vec Nil                  = throw "vec: arg type"
vec _                    = throw "vec: arg type"



-- Hash Map functions

-- Metadata functions



-- Atom functions

atom :: MalFn
atom (v:Nil) = MalAtom <$> liftEffect (Ref.new v)
atom _       = throw "invalid atom call"


atomQ :: MalExpr -> Boolean
atomQ (MalAtom _) = true
atomQ _           = false


deref :: MalFn
deref (MalAtom ref : Nil) = liftEffect $ Ref.read ref
deref _                   = throw "invalid deref call"


resetB :: MalFn
resetB (MalAtom ref : val : Nil) = liftEffect $ Ref.write val ref *> pure val
resetB _                         = throw "invalid reset!"


swapB :: MalFn
swapB (MalAtom ref : MalFunction {fn:f} : args) = do
  val <- liftEffect $ Ref.read ref
  newVal <- f $ val:args
  liftEffect $ Ref.write newVal ref
  pure newVal
swapB _                                         = throw "Illegal swap!"



-- Utils

pred1 :: (MalExpr -> Boolean) -> MalFn
pred1 f (x:Nil) = pure $ MalBoolean $ f x
pred1 _ _       = throw "illegal call to unary predicate"