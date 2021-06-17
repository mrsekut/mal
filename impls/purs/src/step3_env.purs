module Main3 where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (error, log)
import Effect.Exception (throw)
import Env as Env
import Mal.Reader (readStr)
import Printer (printStr)
import Readline (readLine)
import Types (MalExpr(..), MalFn, RefEnv)



-- READ

read :: String -> Either String MalExpr
read = readStr



-- EVAL

eval :: RefEnv -> MalExpr -> Effect MalExpr
eval _ ast@(MalList Nil) = pure ast
eval env (MalList ast)   = case ast of
  (MalSymbol "def!" : es) -> evalDef env es
  (MalSymbol "let*" : es) -> evalLet env es
  _                       -> do
    es <- traverse (evalAst env) ast
    case es of
      (MalFunction {fn:f} : args) -> f args
      _                           -> throw "invalid function"
eval env            ast = evalAst env ast


evalAst :: RefEnv -> MalExpr -> Effect MalExpr
evalAst env (MalSymbol s)     = do
  result <- Env.get env s
  case result of
    Just k  -> pure k
    Nothing -> throw $ "'" <> s <> "'" <> " not found"
evalAst env ast@(MalList _)   = eval env ast
evalAst env (MalVector envs)  = MalVector <$> traverse (eval env) envs
evalAst env (MalHashMap envs) = MalHashMap <$> traverse (eval env) envs
evalAst _ ast                 = pure ast


evalDef :: RefEnv -> List MalExpr -> Effect MalExpr
evalDef env (MalSymbol v : e : Nil) = do
  evd <- evalAst env e
  Env.set env v evd
  pure evd
evalDef _ _                         = throw "invalid def!"


evalLet :: RefEnv -> List MalExpr -> Effect MalExpr
evalLet env (MalList ps : e : Nil)   = do
  letEnv <- Env.newEnv env
  letBind letEnv ps
  evalAst letEnv e
evalLet env (MalVector ps : e : Nil) = do
  letEnv <- Env.newEnv env
  letBind letEnv ps
  evalAst letEnv e
evalLet _ _                          = throw "invalid let*"


letBind :: RefEnv -> List MalExpr -> Effect Unit
letBind _ Nil                       = pure unit
letBind env (MalSymbol ky : e : es) = do
  Env.set env ky =<< evalAst env e
  letBind env es
letBind _ _                         = throw "invalid let*"



-- PRINT

print :: MalExpr -> Effect String
print = printStr



-- REPL

rep :: RefEnv -> String -> Effect String
rep env str = case read str of
  Left _    -> throw "EOF"
  Right ast -> print =<< eval env ast


loop :: RefEnv -> Effect Unit
loop env = do
  line <- readLine
  case line of
    ":q" -> pure unit
    _    -> do
      result <- try $ rep env line
      case result of
        Right exp -> log exp
        Left err  -> error $ show err
      loop env


setArithOp :: RefEnv -> Effect Unit
setArithOp env = do
  Env.set env "+" $ fn (+)
  Env.set env "-" $ fn (-)
  Env.set env "*" $ fn (*)
  Env.set env "/" $ fn (/)


fn :: (Int -> Int -> Int) -> MalExpr
fn op = MalFunction $ { fn : g op, params : Nil }
  where
  g :: (Int -> Int -> Int) -> MalFn
  g op' ((MalInt n1) : (MalInt n2) : Nil) = pure $ MalInt $ op' n1 n2
  g _ _                                   = throw "invalid operator"



--

main :: Effect Unit
main = do
  re <- Env.newEnv Nil
  setArithOp re
  loop re