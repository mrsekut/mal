module Types where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Reader.Trans (ReaderT)
import Data.Array as Array
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map.Internal as Map
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import Effect.Ref as Ref


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


data Key = StringKey String
         | KeywordKey String
derive instance eqKey :: Eq Key
derive instance ordKey :: Ord Key


type MalFn = List MalExpr -> MalEnv MalExpr


type Env = Map String MalExpr
type EnvRef = Ref.Ref (List Env)
newtype MalEnv a = MalEnv (ReaderT EnvRef Effect a)

derive newtype instance functorMalEnv :: Functor MalEnv
derive newtype instance applyMalEnv :: Apply MalEnv
derive newtype instance applicativeMalEnv :: Applicative MalEnv
derive newtype instance bindMalEnv ∷ Bind MalEnv
derive newtype instance monadMalEnv ∷ Monad MalEnv

derive newtype instance monadAskMalEnv :: MonadAsk EnvRef MalEnv
derive newtype instance monadEffectMalEnv :: MonadEffect MalEnv
derive newtype instance monadThrowMalEnv :: MonadThrow Error MalEnv
derive newtype instance monadErrorMalEnv :: MonadError Error MalEnv


-- Utils

toList :: List MalExpr -> MalExpr
toList = MalList


listToMap :: List (Tuple Key MalExpr) -> Map Key MalExpr
listToMap = Map.fromFoldable


charListToString :: List Char -> String
charListToString = fromCharArray <<< Array.fromFoldable


flatTuples :: List (Tuple Key MalExpr) -> List MalExpr
flatTuples ((Tuple (StringKey a) b) : xs)  = MalString a : b : flatTuples xs
flatTuples ((Tuple (KeywordKey a) b) : xs) = MalKeyword a : b : flatTuples xs
flatTuples _                               = Nil
