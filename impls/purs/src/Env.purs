module Env where

import Prelude

import Data.List (List(..), (:))
import Data.Map (fromFoldable, insert, lookup)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (error)
import Effect.Ref as Ref
import Types (Local, MalExpr, RefEnv)



-- FIXME: いる？
-- initEnvRef :: Effect RefEnv
-- initEnvRef = Ref.new $ initEnv:Nil


-- runMalEnv :: ∀ m. MalEnv m -> RefEnv -> Effect m
-- runMalEnv (MalEnv m) = runReaderT m



-- Environment

initEnv :: Local
initEnv = fromFoldable Nil


-- FIXME: clean
newEnv :: RefEnv -> Effect RefEnv
newEnv re = do
  a <- Ref.new initEnv
  pure $ a:re


-- deleteEnv :: MalEnv Unit
-- deleteEnv = do
--   ref <- ask
--   envs <- liftEffect $ Ref.read ref
--   case tail envs of
--     Just es -> liftEffect $ Ref.write es ref
--     Nothing -> liftEffect $ Ref.write envs ref


-- local :: ∀ a. MalEnv a -> MalEnv a
-- local cb = do
--   newEnv
--   result <- cb
--   deleteEnv
--   pure result



-- VARIABLE

get :: RefEnv -> String -> Effect (Maybe MalExpr)
get Nil _ = pure Nothing
get (ref:outer) ky = do
  envs <- Ref.read ref
  case lookup ky envs of
    Nothing -> get outer ky
    ex      -> pure ex


-- sets :: List String -> List MalExpr -> MalEnv Boolean
-- sets Nil Nil           = pure true
-- sets ("&":k:Nil) exs   = set k (toList exs) *> pure true
-- sets (ky:kys) (ex:exs) = set ky ex *> sets kys exs
-- sets _ _               = pure false


set :: RefEnv -> String -> MalExpr -> Effect Unit
set (re:_) ky ex = Ref.modify_ (insert ky ex) re
set Nil _ _      = error "assertion failed in env_set"
