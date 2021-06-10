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

derive newtype instance functorMalEnv :: Functor MalEnv
derive newtype instance applyMalEnv :: Apply MalEnv
derive newtype instance applicativeMalEnv :: Applicative MalEnv
derive newtype instance bindMalEnv ∷ Bind MalEnv

derive newtype instance monadAskMalEnv :: MonadAsk EnvRef MalEnv
derive newtype instance monadEffectMalEnv :: MonadEffect MalEnv
derive newtype instance monadThrowMalEnv :: MonadThrow Error MalEnv
derive newtype instance monadErrorMalEnv :: MonadError Error MalEnv



initEnvRef :: Effect EnvRef
initEnvRef = Ref.new $ initEnv:Nil


runMalEnv :: ∀ m. MalEnv m -> EnvRef -> Effect m
runMalEnv (MalEnv m) = runReaderT m



-- Environment

initEnv :: Env
initEnv = fromFoldable Nil


newEnv :: MalEnv Unit
newEnv = do
  ref <- ask
  envs <- liftEffect $ Ref.read ref
  liftEffect $ Ref.write (initEnv:envs) ref


deleteEnv :: MalEnv Unit
deleteEnv = do
  ref <- ask
  envs <- liftEffect $ Ref.read ref
  case tail envs of
    Just es -> liftEffect $ Ref.write es ref
    Nothing -> liftEffect $ Ref.write envs ref


local :: ∀ a. MalEnv a -> MalEnv a
local cb = do
  newEnv
  result <- cb
  deleteEnv
  pure result



-- VARIABLE

get :: String -> List Env -> Maybe MalExpr
get ky envs = case find ky envs of
    Just ex -> lookup ky ex
    Nothing -> Nothing
  where

  find :: String -> List Env -> Maybe Env
  find _ Nil       = Nothing
  find ky' (env:outer)
    | member ky' env = Just env
    | otherwise      = find ky' outer


set :: String -> MalExpr -> MalEnv Unit
set ky ex = do
  ref <- ask
  env <- liftEffect $ Ref.read ref
  liftEffect $ Ref.write (update ky ex env) ref
  where

  update :: String -> MalExpr -> List Env -> List Env
  update _ _ Nil             = Nil
  update ky' ex' (env:outer) = (insert ky' ex' env):outer