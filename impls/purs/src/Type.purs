module Types where

import Prelude

import Control.Monad.Free.Trans (FreeT, runFreeT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Identity (Identity(..))
import Data.List (List(..), foldr, (:))
import Data.List as List
import Data.Map (Map, toUnfoldable)
import Data.Map.Internal as Map
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.CodeUnits (singleton)
import Data.Traversable (foldl)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)

runIdentity :: forall a. Identity a -> a
runIdentity (Identity a) = a

type SafeT = FreeT Identity

runSafeT :: forall m a. (MonadRec m) => SafeT m a -> m a
runSafeT = runFreeT (pure <<< runIdentity)


data MalExpr
  = MalNil
  | MalBoolean Boolean
  | MalInt Int
  | MalString String
  | MalKeyword String
  | MalSymbol String
  | MalAtom Meta (Ref MalExpr)
  | MalList Meta (List MalExpr)
  | MalVector Meta (List MalExpr)
  | MalHashMap Meta (Map Key MalExpr)
  | MalFunction { fn :: MalFn
                , ast :: MalExpr
                , env :: RefEnv
                , params :: List String
                , macro :: Boolean
                , meta :: MalExpr
                }

instance Eq MalExpr where
  eq MalNil MalNil                     = true
  eq (MalBoolean a) (MalBoolean b)     = a == b
  eq (MalInt a) (MalInt b)             = a == b
  eq (MalString a) (MalString b)       = a == b
  eq (MalKeyword a) (MalKeyword b)     = a == b
  eq (MalSymbol a) (MalSymbol b)       = a == b

  eq (MalList _ a) (MalList _ b)       = a == b
  eq (MalVector _ a) (MalList _ b)     = a == b
  eq (MalList _ a) (MalVector _ b)     = a == b

  eq (MalVector _ a) (MalVector _ b)   = a == b
  eq (MalHashMap _ a) (MalHashMap _ b) = a == b
  eq _ _                               = false


data Key = StringKey String
         | KeywordKey String

derive instance Eq Key
derive instance Ord Key


type MalFn = List MalExpr -> Effect MalExpr


type Local = Map String MalExpr
type RefEnv = List (Ref.Ref Local)



-- Metas

newtype Meta = Meta MalExpr


toList :: List MalExpr -> MalExpr
toList = MalList (Meta MalNil)


toVector :: List MalExpr -> MalExpr
toVector = MalVector (Meta MalNil)


toAtom :: Ref MalExpr -> MalExpr
toAtom = MalAtom (Meta MalNil)


toHashMap :: Map Key MalExpr -> MalExpr
toHashMap = MalHashMap (Meta MalNil)



-- Utils

listToMap :: List (Tuple Key MalExpr) -> Map Key MalExpr
listToMap = Map.fromFoldable


charListToString :: List Char -> String
charListToString = fromCharArray <<< Array.fromFoldable


stringToCharList :: String -> List Char
stringToCharList = List.fromFoldable <<< toCharArray


flatStrings :: List String -> String
flatStrings = foldr (<>) ""


flatTuples :: List (Tuple Key MalExpr) -> List MalExpr
flatTuples ((Tuple (StringKey a) b) : xs)  = MalString a : b : flatTuples xs
flatTuples ((Tuple (KeywordKey a) b) : xs) = MalKeyword a : b : flatTuples xs
flatTuples _                               = Nil


foldrM :: forall a m b f. Foldable f => Monad m => (a -> b -> m b) -> b -> f a -> m b
foldrM f z0 xs = foldl c pure xs z0
  where c k x z = f x z >>= k


keyToString :: Key -> MalExpr
keyToString (StringKey k)  = MalString k
keyToString (KeywordKey k) = MalKeyword k


keyValuePairs :: List MalExpr -> Maybe (List (Tuple String MalExpr))
keyValuePairs Nil                     = pure Nil
keyValuePairs (MalString k : v : kvs) = (:)  (Tuple k v) <$> keyValuePairs kvs
keyValuePairs _                       = Nothing



instance Show MalExpr where
  show = unsafePerformEffect <<< printStr

printStr :: MalExpr -> Effect String
printStr MalNil           = pure "nil"
printStr (MalBoolean b)   = pure $ show b
printStr (MalInt n)       = pure $ show n
printStr (MalString str)  = pure $ "\"" <> (str # stringToCharList # map unescape # flatStrings) <> "\""
printStr (MalKeyword key) = pure key
printStr (MalAtom _ r)      = "(atom " <<> (Ref.read r >>= printStr) <>> ")"
printStr (MalSymbol name)   = pure name
printStr (MalList _ xs)     = "(" <<> printList xs <>> ")"
printStr (MalVector _ vs)   = "[" <<> printList vs <>> "]"
printStr (MalHashMap _ hm)  = "{" <<> (hm # toUnfoldable # flatTuples # printList) <>> "}"
printStr (MalFunction _)    = pure "#<function>"


printList :: List MalExpr -> Effect String
printList Nil     = pure ""
printList (x:Nil) = printStr x
printList (x:xs)  = printStr x <> pure " " <> printList xs

unescape :: Char -> String
unescape '\n' = "\\n"
unescape '\\' = "\\\\"
unescape '"'  = "\\\""
unescape c    = singleton c


leftConcat :: forall m s. Bind m => Applicative m => Semigroup s => s -> m s -> m s
leftConcat op f = (<>) <$> pure op <*> f

infixr 5 leftConcat as <<>


rightConcat :: forall m s. Apply m => Semigroup s => Applicative m => m s -> s -> m s
rightConcat f cl = (<>) <$> f <*> pure cl

infixr 5 rightConcat as <>>