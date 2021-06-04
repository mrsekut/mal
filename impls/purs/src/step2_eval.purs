module Main where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Map (Map, lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (error, log)
import Mal.Reader (readStr)
import Printer (printStr)
import Readline (readLine)
import Types (MalExpr(..), MalFn)


-- READ

read :: String -> Either String MalExpr
read = readStr



-- EVAL

eval :: ReplEnv -> MalExpr -> MalExpr
eval _ ast@(MalList Nil)  = ast
eval env (MalList ast)    = case map (evalAst env) ast of
  (MalFunction {fn:f}: args) -> f args
  _                          -> MalNil      -- FIXME: error
eval env ast              = evalAst env ast



evalAst :: ReplEnv -> MalExpr -> MalExpr
evalAst env (MalSymbol s)   = case lookup s env of
  Just f -> f
  Nothing -> MalNil -- FIXME: error
evalAst env a@(MalList _)    = eval env a
-- evalAst env a@(MalVector w)  = MalVector $ MalInt 1 : Nil
evalAst env a@(MalVector w)  = MalVector $ map (eval env) w
evalAst env (MalHashMap es) = MalHashMap $ map (eval env) es
evalAst _ ast               = ast


-- PRINT

print :: String -> String
print s = s



-- ENV

type ReplEnv = Map String MalExpr

replEnv :: ReplEnv
replEnv = Map.fromFoldable
  [ (Tuple "+" (fn (+)))
  , (Tuple "-" (fn (-)))
  , (Tuple "*" (fn (*)))
  , (Tuple "/" (fn (/)))
  ]

fn :: (Int -> Int -> Int) -> MalExpr
fn op = MalFunction $ { fn : g op }
  where
  g :: (Int -> Int -> Int) -> MalFn
  g op' ((MalInt n1) : (MalInt n2) : Nil) = MalInt $ op' n1 n2
  g _ _                                  = MalInt 42 -- FIXME: error


-- REPL

rep :: String -> Effect Unit
rep str = case read str of
  Left _ -> error "EOF"
  Right s -> log $ print $ printStr $ eval replEnv s

loop :: Aff Unit
loop = do
  line <- readLine "user> "
  case line of
    ":q" -> pure unit
    ":Q" -> pure unit
    _    -> do
      liftEffect $ rep line
      loop



--

main :: Effect Unit
main = do
  launchAff_ loop
