module Main4 where

import Prelude

import Control.Monad.Error.Class (try)
import Core as Core
import Data.Either (Either(..))
import Data.List (List(..), foldM, (:))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
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
  MalSymbol "def!" : es -> evalDef env es
  MalSymbol "let*" : es -> evalLet env es
  MalSymbol "if" : es   -> evalIf env es
  MalSymbol "do" : es   -> evalDo env es
  MalSymbol "fn*" : es  -> evalFnMatch env es
  _                     -> do
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


evalIf :: RefEnv -> List MalExpr -> Effect MalExpr
evalIf env (b:t:e:Nil) = do
  cond <- evalAst env b
  evalAst env case cond of
    MalNil           -> e
    MalBoolean false -> e
    _                -> t
evalIf env (b:t:Nil)   = do
  cond <- evalAst env b
  evalAst env case cond of
    MalNil           -> MalNil
    MalBoolean false -> MalNil
    _                -> t
evalIf _ _             = throw "invalid if"


evalDo :: RefEnv -> List MalExpr -> Effect MalExpr
evalDo env es = foldM (const $ evalAst env) MalNil es


evalFnMatch :: RefEnv -> List MalExpr -> Effect MalExpr
evalFnMatch env (MalList params : body : Nil)   = evalFn env params body
evalFnMatch env (MalVector params : body : Nil) = evalFn env params body
evalFnMatch _ _                                 = throw "invalid fn*"


evalFn :: RefEnv -> List MalExpr -> MalExpr -> Effect MalExpr
evalFn env params body = do
  paramsStr <- traverse unwrapSymbol params
  pure $ MalFunction { fn : fn paramsStr body, params : paramsStr }
  where

  fn :: List String -> MalExpr -> MalFn
  fn params' body' = \args -> do
    fnEnv <- Env.newEnv env
    ok <- Env.sets fnEnv params' args
    if ok
      then evalAst fnEnv body'
      else throw "actual parameters do not match signature "

  unwrapSymbol :: MalExpr -> Effect String
  unwrapSymbol (MalSymbol s) = pure s
  unwrapSymbol _             = throw "fn* parameter must be symbols"



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


setFn :: RefEnv -> Tuple String MalFn -> Effect Unit
setFn env (Tuple sym f) = Env.set env sym $ MalFunction { fn : f, params : Nil }



--

main :: Effect Unit
main = do
  re <- Env.newEnv Nil
  _ <- traverse (setFn re) Core.ns
  _ <- rep re "(def! not (fn* (a) (if a false true)))"
  loop re
