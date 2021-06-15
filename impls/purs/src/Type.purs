module Types where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Reader.Trans (ReaderT)
import Data.Array as Array
import Data.List (List(..), foldr, (:))
import Data.List as List
import Data.Map (Map, toUnfoldable)
import Data.Map.Internal as Map
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import Effect.Ref as Ref
import Data.String.CodeUnits(singleton)



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
  | MalFunction { fn :: MalFn , params :: List String }

instance Eq MalExpr where
  eq MalNil MalNil                 = true
  eq (MalBoolean a) (MalBoolean b) = a == b
  eq (MalInt a) (MalInt b)         = a == b
  eq (MalString a) (MalString b)   = a == b
  eq (MalKeyword a) (MalKeyword b) = a == b
  eq (MalSymbol a) (MalSymbol b)   = a == b

  eq (MalList a) (MalList b)       = a == b
  eq (MalVector a) (MalList b)     = a == b
  eq (MalList a) (MalVector b)     = a == b

  eq (MalVector a) (MalVector b)   = a == b
  eq (MalHashMap a) (MalHashMap b) = a == b
  eq _ _                           = false


data Key = StringKey String
         | KeywordKey String

derive instance Eq Key
derive instance Ord Key


type MalFn = List MalExpr -> MalEnv MalExpr


type Env = Map String MalExpr
type EnvRef = Ref.Ref (List Env)
newtype MalEnv a = MalEnv (ReaderT EnvRef Effect a)

derive newtype instance Functor MalEnv
derive newtype instance Apply MalEnv
derive newtype instance Applicative MalEnv
derive newtype instance Bind MalEnv
derive newtype instance Monad MalEnv

derive newtype instance MonadAsk EnvRef MalEnv
derive newtype instance MonadEffect MalEnv
derive newtype instance MonadThrow Error MalEnv
derive newtype instance MonadError Error MalEnv



-- Utils

toList :: List MalExpr -> MalExpr
toList = MalList


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












instance Show MalExpr where
  show = printStr

printStr :: MalExpr -> String
printStr MalNil           = "nil"
printStr (MalBoolean b)   = show b
printStr (MalInt n)       = show n
printStr (MalString str)  = "\"" <> (str # stringToCharList # map unescape # flatStrings) <> "\""
printStr (MalKeyword key) = key
printStr (MalSymbol name) = name
printStr (MalList xs)     = "(" <> printList xs <> ")"
printStr (MalVector vs)   = "[" <> printList vs <> "]"
printStr (MalHashMap hm)  = "{" <> (hm # toUnfoldable # flatTuples # printList) <> "}"
printStr (MalFunction _)  = "#<function>"


printList :: List MalExpr -> String
printList Nil     = ""
printList (x:Nil) = printStr x
printList (x:xs)  = printStr x <> " " <> printList xs

unescape :: Char -> String
unescape '\n' = "\\n"
unescape '\\' = "\\\\"
unescape '"'  = "\\\""
unescape c    = singleton c

