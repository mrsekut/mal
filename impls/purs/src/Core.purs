module Core (ns) where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..), concat, drop, foldM, fromFoldable, length, (:))
import Data.Map.Internal as Map
import Data.Maybe (Maybe(..))
import Data.String (take)
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
import Printer (keyValuePairs, printList, printListReadably, printStrReadably)
import Types (Key(..), MalExpr(..), MalFn, keyToString)



ns :: List (Tuple String MalFn)
ns = fromFoldable
  [ Tuple "throw"      throw'

  , Tuple "true?"       $ pred1 trueQ
  , Tuple "false?"      $ pred1 falseQ

  , Tuple "="           eqQ
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

  , Tuple "symbol?"     $ pred1 symbolQ
  , Tuple "symbol"      symbol
  , Tuple "keyword?"    $ pred1 keywordQ
  , Tuple "keyword"     keyword

  , Tuple "list"        list
  , Tuple "list?"       $ pred1 listQ
  , Tuple "nil?"        $ pred1 nilQ
  , Tuple "empty?"      $ pred1 emptyQ
  , Tuple "count"       count
  , Tuple "sequential?" $ pred1 sequentialQ
  , Tuple "cons"        cons
  , Tuple "concat"      concat'
  , Tuple "nth"         nth
  , Tuple "first"       first
  , Tuple "rest"        rest
  , Tuple "apply"       apply'
  , Tuple "map"         map'
  , Tuple "map?"        $ pred1 mapQ

  , Tuple "vec"         vec
  , Tuple "vector"      vector
  , Tuple "vector?"     $ pred1 vectorQ

  , Tuple "hash-map"    hashMap
  , Tuple "assoc"       assoc
  , Tuple "dissoc"      dissoc
  , Tuple "get"         get
  , Tuple "contains?"   containsQ
  , Tuple "keys"        keys
  , Tuple "vals"        vals

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

throw' :: MalFn
throw' (e:Nil) = throw =<< printStrReadably e
throw' _       = throw "illegal arguments to throw"



-- Boolean functions

trueQ :: MalExpr -> Boolean
trueQ (MalBoolean true) = true
trueQ _                 = false


falseQ :: MalExpr -> Boolean
falseQ (MalBoolean false) = true
falseQ _                  = false


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


str :: MalFn
str a = liftEffect $ MalString <$> printListReadably "" a


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



-- Scalar functions

symbolQ :: MalExpr -> Boolean
symbolQ (MalSymbol _) = true
symbolQ _             = false


symbol :: MalFn
symbol (MalString s : Nil) = pure $ MalSymbol s
symbol _                   = throw "symbol called with non-string"


keywordQ :: MalExpr -> Boolean
keywordQ (MalKeyword s) = take 1 s == ":"
keywordQ _             = false


keyword :: MalFn
keyword (kw@(MalString s) : Nil) | take 1 s == ":" = pure kw
keyword (MalString s : Nil)  = pure $ MalKeyword (":" <> s)
keyword (kw@(MalKeyword s) : Nil) | take 1 s == ":" = pure kw
keyword (MalKeyword s : Nil) = pure $ MalKeyword (":" <> s)
keyword _                    = throw "keyword called with non-string"



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


sequentialQ :: MalExpr -> Boolean
sequentialQ (MalList _)   = true
sequentialQ (MalVector _) = true
sequentialQ _             = false


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


nth :: MalFn
nth (MalList xs : MalInt n : Nil)   =
  case drop n xs of
    x:_ -> pure x
    Nil -> throw "nth: index out of range"
nth (MalVector xs : MalInt n : Nil) =
  case drop n xs of
    x:_ -> pure x
    Nil -> throw "nth: index out of range"
nth _                               = throw "invalid call to nth"


first :: MalFn
first (MalNil:Nil)            = pure MalNil
first (MalList Nil : Nil)     = pure MalNil
first (MalList (x:_) : Nil)   = pure x
first (MalVector Nil : Nil)   = pure MalNil
first (MalVector (x:_) : Nil) = pure x
first _                       = throw "illegal call to first"


rest :: MalFn
rest (MalNil:Nil)             = pure $ MalList Nil
rest (MalList Nil : Nil)      = pure $ MalList Nil
rest (MalList (_:xs) : Nil)   = pure $ MalList xs
rest (MalVector Nil : Nil)    = pure $ MalList Nil
rest (MalVector (_:xs) : Nil) = pure $ MalList xs
rest _                        = throw "illegal call to rest"


apply' :: MalFn
apply' (MalFunction {fn:f} : as) = f =<< concatLast as
  where
  concatLast :: List MalExpr -> Effect (List MalExpr)
  concatLast (MalList lst : Nil)   = pure lst
  concatLast (MalVector lst : Nil) = pure lst
  concatLast (x:xs)                = (:) x <$> concatLast xs
  concatLast _                     = throw "last argument of apply must be a sequence"
apply' _ = throw "Illegal call to apply"


map' :: MalFn
map' (MalFunction {fn:f} : MalList args : Nil)   = MalList <$> traverse (\x -> f (x:Nil)) args
map' (MalFunction {fn:f} : MalVector args : Nil) = MalList <$> traverse (\x -> f (x:Nil)) args
map' _ = throw "Illegal call to map"


mapQ :: MalExpr -> Boolean
mapQ (MalHashMap _) = true
mapQ _              = false


-- Vector functions

vec :: MalFn
vec (MalList xs : Nil)   = pure $ MalVector xs
vec (MalVector xs : Nil) = pure $ MalVector xs
vec Nil                  = throw "vec: arg type"
vec _                    = throw "vec: arg type"


vector :: MalFn
vector = pure <<< MalVector


vectorQ :: MalExpr -> Boolean
vectorQ (MalVector _) = true
vectorQ _             = false



-- Hash Map functions

hashMap :: MalFn
hashMap kvs =
  case keyValuePairs kvs of
    Just pairs -> pure $ MalHashMap $ Map.fromFoldable pairs
    Nothing    -> throw "invalid call to hash-map"


assoc :: MalFn
assoc (MalHashMap hm : kvs) =
    case keyValuePairs kvs of
        Just pairs -> pure $ MalHashMap  $ Map.union (Map.fromFoldable pairs) hm
        Nothing    -> throw "invalid assoc"
assoc _ = throw "invalid call to assoc"


dissoc :: MalFn
dissoc (MalHashMap hm : ks) = MalHashMap <$> foldM remover hm ks
  where
  remover :: Map.Map Key MalExpr -> MalExpr -> Effect (Map.Map Key MalExpr)
  remover m (MalKeyword k) = pure $ Map.delete (KeywordKey k) m
  remover m (MalString k)  = pure $ Map.delete (StringKey k) m
  remover _ _              = throw "invalid dissoc"
dissoc _ = throw "invalid call to dissoc"


get :: MalFn
get (MalHashMap hm : MalString k : Nil)  =
  pure case Map.lookup (StringKey k) hm of
    Just mv -> mv
    Nothing -> MalNil
get (MalHashMap hm : MalKeyword k : Nil) =
  pure case Map.lookup (KeywordKey k) hm of
    Just mv -> mv
    Nothing -> MalNil
get (MalNil : MalString _ : Nil)         = pure MalNil
get _                                    = throw "invalid call to get"


containsQ :: MalFn
containsQ (MalHashMap hm : MalString k : Nil)  = pure $ MalBoolean $ Map.member (StringKey k) hm
containsQ (MalHashMap hm : MalKeyword k : Nil) = pure $ MalBoolean $ Map.member (KeywordKey k) hm
containsQ (MalNil : MalString _ : Nil)         = pure $ MalBoolean false
containsQ _                                    = throw "invalid call to contains?"


keys :: MalFn
keys (MalHashMap hm : Nil) = pure $ MalList $ keyToString <$> Map.keys hm
keys _                     = throw "invalid call to keys"


vals :: MalFn
vals (MalHashMap hm : Nil) = pure $ MalList $ Map.values hm
vals _                     = throw "invalid call to vals"



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