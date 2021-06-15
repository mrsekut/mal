module Core (ns) where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..), fromFoldable, (:), length)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Console (log)
import Env (throwStr)
import Mal.Reader (readStr)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Printer (printListReadably, printList)
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
  ]



-- General functions

eqQ :: MalFn
eqQ (a:b:Nil) = pure $ MalBoolean $ a == b
eqQ _         = throwStr "illegal arguments to ="



-- Error/Exception functions



-- Scalar functions



-- Numeric functions

numOp :: (Int -> Int -> Int) -> MalFn
numOp op ((MalInt n1) : (MalInt n2) : Nil) = pure $ MalInt $ op n1 n2
numOp _ _                                  = throwStr "invalid operator"


cmpOp :: (Int -> Int -> Boolean) -> MalFn
cmpOp op ((MalInt n1) : (MalInt n2) : Nil) = pure $ MalBoolean $ op n1 n2
cmpOp _ _                                  = throwStr "invalid operator"



-- String functions

prStr :: MalFn
prStr = pure <<< MalString <<< printList


str :: MalFn
str = pure <<< MalString <<< printListReadably ""


prn :: MalFn
prn args = liftEffect $ do
  log $ printList args
  pure MalNil


println :: MalFn
println args = liftEffect $ do
  log $ printListReadably " " args
  pure MalNil


slurp :: MalFn
slurp (MalString path : Nil) = MalString <$> liftEffect (readTextFile UTF8 path)
slurp _                      = throwStr "invalid arguments to slurp"


readString :: MalFn
readString (MalString s : Nil) = case readStr s of
  Left _    -> throwStr "invalid read-string"
  Right val ->  pure val
readString _                   = throwStr "invalid read-string"



-- List functions

list :: MalFn
list = pure <<< toList


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
count _                    = throwStr "non-sequence passed to count"



-- Vector functions

-- Hash Map functions

-- Metadata functions

-- Atom functions



-- atom :: MalFn
-- atom [val] = MalAtom (MetaData Nil) <$> liftIO (newIORef val)
-- atom _ = throwStr "invalid atom call"


-- atom_Q :: MalVal -> Bool
-- atom_Q (MalAtom _ _) = True
-- atom_Q _             = False


-- deref :: Fn
-- deref [MalAtom _ ref] = liftIO $ readIORef ref
-- deref _ = throwStr "invalid deref call"


-- reset_BANG :: Fn
-- reset_BANG [MalAtom _ ref, val] = do
--     liftIO $ writeIORef ref $ val
--     return val
-- reset_BANG _ = throwStr "invalid reset!"


-- swap_BANG :: Fn
-- swap_BANG (MalAtom _ ref : MalFunction {fn=f} : args) = do
--     val <- liftIO $ readIORef ref
--     new_val <- f (val : args)
--     liftIO $ writeIORef ref new_val
--     return new_val
-- swap_BANG _ = throwStr "Illegal swap!"


-- Utils

pred1 :: (MalExpr -> Boolean) -> MalFn
pred1 f (x:Nil) = pure $ MalBoolean $ f x
pred1 _ _       = throwStr "illegal call to unary predicate"