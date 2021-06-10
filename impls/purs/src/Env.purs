module Env where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Reader.Trans (ReaderT, ask, runReaderT)
import Data.List (List(..), tail, (:))
import Data.Map (Map, fromFoldable, insert, lookup, member)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Effect.Ref as Ref
import Types (MalExpr)


type Env = Map String MalExpr
type EnvRef = Ref.Ref (List Env)

newtype MalEnv a = MalEnv (ReaderT EnvRef Effect a)
derive newtype instance bindMalEnv ∷ Bind MalEnv
derive newtype instance monadAskMalEnv :: MonadAsk EnvRef MalEnv
derive newtype instance functorMalEnv :: Functor MalEnv
derive newtype instance applicativeMalEnv :: Applicative MalEnv
derive newtype instance monadEffectMalEnv :: MonadEffect MalEnv
-- derive newtype instance monadtMalEnv :: Monad MalEnv

-- derive newtype instance monadtMalEnv :: Monad MalEnv
derive newtype instance monadThrowMalEnv :: MonadThrow Error MalEnv

-- instance monadThrowMalEnv :: MonadThrow Error MalEnv where
--   throwError = liftEffect <<< throwException

derive newtype instance monadErrorMalEnv :: MonadError Error MalEnv
--   catchError (MalEnv e) h = MalEnv e


get :: String -> List Env -> Maybe MalExpr
get k es = case find k es of
    Just e  -> lookup k e
    Nothing -> Nothing


find :: String -> List Env -> Maybe Env
find _ Nil       = Nothing
find k (e:outer) =
  if member k e
    then Just e
    else find k outer


set :: String -> MalExpr -> MalEnv Unit
set k e = do
  ref <- ask
  env <- liftEffect $ Ref.read ref
  liftEffect $ Ref.write (update k e env) ref


update :: String -> MalExpr -> List Env -> List Env
update _ _ Nil         = Nil
update k e (env:outer) = (insert k e env):outer


newEnv :: MalEnv Unit
newEnv = do
  ref <- ask
  es <- liftEffect $ Ref.read ref
  liftEffect $ Ref.write (initEnv:es) ref


deleteEnv :: MalEnv Unit
deleteEnv = do
  ref <- ask
  ees <- liftEffect $ Ref.read ref
  case tail ees of
    Just es -> liftEffect $ Ref.write es ref
    Nothing -> liftEffect $ Ref.write ees ref


-- local :: () -> MalEnv MalExpr

make :: Effect EnvRef
make = Ref.new $ initEnv:Nil


initEnv :: Env
initEnv = fromFoldable Nil


runMalEnv :: ∀ m. MalEnv m -> EnvRef -> Effect m
runMalEnv (MalEnv m) = runReaderT m

