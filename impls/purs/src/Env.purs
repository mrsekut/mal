module Env where

import Prelude

import Control.Monad.Reader.Trans (ask, runReaderT)
import Data.List (List(..), last, tail, (:))
import Data.Map (fromFoldable, insert, lookup, member)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Types (Env, EnvRef, MalEnv(..), MalExpr, toList)


initEnvRef :: Effect EnvRef
initEnvRef = Ref.new $ initEnv:Nil


runMalEnv :: ∀ m. MalEnv m -> EnvRef -> Effect m
runMalEnv (MalEnv m) = runReaderT m



-- Environment

initEnv :: Env
initEnv = fromFoldable Nil


resetEnv :: MalEnv Unit
resetEnv = do
  ref <- ask
  envs <- liftEffect $ Ref.read ref
  case last envs of
    Just e -> liftEffect $ Ref.write ( e:Nil ) ref
    Nothing -> liftEffect $ Ref.write envs ref


setEnv :: List Env -> MalEnv Unit
setEnv env = do
  ref <- ask
  liftEffect $ Ref.write env ref

newEnv :: MalEnv Unit
newEnv = do
  ref <- ask
  envs <- liftEffect $ Ref.read ref
  liftEffect $ log $ show "=====new========"
  liftEffect $ Ref.write (initEnv:envs) ref


deleteEnv :: MalEnv Unit
deleteEnv = do
  ref <- ask
  envs <- liftEffect $ Ref.read ref
  liftEffect $ log $ show "=====delte========"
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


sets :: List String -> List MalExpr -> MalEnv Boolean
sets Nil Nil           = pure true
sets ("&":k:Nil) exs   = set k (toList exs) *> pure true
sets (ky:kys) (ex:exs) = set ky ex *> sets kys exs
sets _ _               = pure false


set :: String -> MalExpr -> MalEnv Unit
set ky ex = do
  ref <- ask
  envs <- liftEffect $ Ref.read ref
  liftEffect $ Ref.write (update ky ex envs) ref
  ref <- ask
  envs <- liftEffect $ Ref.read ref
  liftEffect $ log $ show "=====set========"
  liftEffect $ log $ show envs
  where

  update :: String -> MalExpr -> List Env -> List Env
  update _ _ Nil             = Nil
  update ky' ex' (env:outer) = (insert ky' ex' env):outer



-- UTILS

throwStr :: forall m a. MonadEffect m => String -> m a
throwStr = liftEffect <<< throw