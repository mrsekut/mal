module Env where

import Prelude

import Data.List (List(..), (:))
import Data.Map (fromFoldable, insert, lookup, member)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref as Ref
import Types (Local, MalExpr, RefEnv)



-- FIXME: いる？
initEnvRef :: Effect RefEnv
initEnvRef = Ref.new $ initEnv:Nil


-- runMalEnv :: ∀ m. MalEnv m -> RefEnv -> Effect m
-- runMalEnv (MalEnv m) = runReaderT m



-- Environment

-- FIXME: いる？
initEnv :: Local
initEnv = fromFoldable Nil


newEnv :: RefEnv -> Effect RefEnv
newEnv re = do
  envs <- Ref.read re
  Ref.write (initEnv:envs) re
  pure re


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
get re ky = do
  envs <- Ref.read re
  case find ky envs of
    Just ex -> pure $ lookup ky ex
    Nothing -> pure Nothing
  where

  find :: String -> List Local -> Maybe Local
  find _ Nil       = Nothing
  find ky' (env:outer)
    | member ky' env = Just env
    | otherwise      = find ky' outer


-- sets :: List String -> List MalExpr -> MalEnv Boolean
-- sets Nil Nil           = pure true
-- sets ("&":k:Nil) exs   = set k (toList exs) *> pure true
-- sets (ky:kys) (ex:exs) = set ky ex *> sets kys exs
-- sets _ _               = pure false


set :: RefEnv -> String -> MalExpr -> Effect Unit
set re ky ex = do
  envs <- Ref.read re
  Ref.write (update ky ex envs) re
  where

  update :: String -> MalExpr -> List Local -> List Local
  update _ _ Nil             = Nil
  update ky' ex' (env:outer) = (insert ky' ex' env):outer