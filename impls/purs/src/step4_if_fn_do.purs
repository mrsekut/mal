module Main where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Reader.Class (ask)
import Data.Either (Either(..))
import Data.List (List(..), foldM, (:))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (error, log)
import Effect.Ref as Ref
import Env (throwStr)
import Env as Env
import Mal.Reader (readStr)
import Printer (printStr)
import Readline (readLine)
import Types (MalEnv, MalExpr(..), MalFn)



-- READ

read :: String -> Either String MalExpr
read = readStr



-- EVAL

eval :: MalExpr -> MalEnv MalExpr
eval ast@(MalList Nil) = pure ast
eval (MalList ast)     = case ast of
  MalSymbol "def!" : es -> evalDef es
  MalSymbol "let*" : es -> evalLet es
  MalSymbol "do" : es   -> evalDo es
  MalSymbol "if" : es   -> evalIf es
  MalSymbol "fn*" : es  -> evalFn es
  _                     -> do
    es <- traverse evalAst ast
    case es of
      (MalFunction {fn:f} : args) -> f args
      _                           -> throwStr "invalid function"
eval ast               = evalAst ast


evalAst :: MalExpr -> MalEnv MalExpr
evalAst (MalSymbol s)   = do
  ref <- ask
  envs <- liftEffect $ Ref.read ref
  case Env.get s envs of
    Just k  -> pure k
    Nothing -> throwStr $ "'" <> s <> "'" <> " not found"
evalAst ast@(MalList _) = eval ast
evalAst (MalVector es)  = MalVector <$> traverse eval es
evalAst (MalHashMap es) = MalHashMap <$> traverse eval es
evalAst ast             = pure ast


evalDef :: List MalExpr -> MalEnv MalExpr
evalDef (MalSymbol v : e : Nil) = do
  evd <- evalAst e
  Env.set v evd
  pure evd
evalDef _                         = throwStr "invalid def!"


evalLet :: List MalExpr -> MalEnv MalExpr
evalLet (MalList ps : e : Nil)   = Env.local $ letBind ps *> eval e
evalLet (MalVector ps : e : Nil) = Env.local $ letBind ps *> eval e
evalLet _                        = throwStr "invalid let*"


letBind :: List MalExpr -> MalEnv Unit
letBind Nil                     = pure unit
letBind (MalSymbol ky : e : es) = (Env.set ky =<< eval e) *> letBind es
letBind _                       = throwStr "invalid let*"


evalIf :: List MalExpr -> MalEnv MalExpr
evalIf (b:t:e:Nil) = do
  cond <- evalAst b
  evalAst case cond of
    MalNil           -> e
    MalBoolean false -> e
    _                -> t
evalIf (b:t:Nil)   = do
  cond <- evalAst b
  evalAst case cond of
    MalNil           -> MalNil
    MalBoolean false -> MalNil
    _                -> t
evalIf _           = throwStr "invalid if"


evalDo :: List MalExpr -> MalEnv MalExpr
evalDo es = foldM (const evalAst) MalNil es


evalFn :: List MalExpr -> MalEnv MalExpr
evalFn (MalList params : body : Nil) = do
  strParams <- traverse unwrapSymbol params
  pure $ MalFunction { fn : fn strParams body, params : strParams }
  where

  fn :: List String -> MalExpr -> MalFn
  fn params' body' = \args -> Env.local do
    ok <- Env.sets params' args
    if ok
      then evalAst body'
      else throwStr "actual parameters do not match signature "
evalFn _                             = throwStr "invalid fn*"


unwrapSymbol :: MalExpr -> MalEnv String
unwrapSymbol (MalSymbol s) = pure s
unwrapSymbol _             = throwStr "fn* parameter must be symbols"



-- PRINT

print :: MalExpr -> String
print = printStr



-- REPL

rep :: String -> MalEnv String
rep str = case read str of
  Left _    -> throwStr "EOF"
  Right ast -> print <$> eval ast


loop :: MalEnv Unit
loop = do
  line <- liftEffect readLine
  case line of
    ":q" -> pure unit
    _    -> do
      result <- try $ rep line
      case result of
        Right exp -> liftEffect $ log exp
        Left err  -> liftEffect $ error $ show err
      loop


setArithOp :: MalEnv Unit
setArithOp = do
  Env.set "+" $ fn (+)
  Env.set "-" $ fn (-)
  Env.set "*" $ fn (*)
  Env.set "/" $ fn (/)
  where

  fn :: (Int -> Int -> Int) -> MalExpr
  fn op = MalFunction $ { fn : g op, params : Nil }
    where
    g :: (Int -> Int -> Int) -> MalFn
    g op' ((MalInt n1) : (MalInt n2) : Nil) = pure $ MalInt $ op' n1 n2
    g _ _                                   = throwStr "invalid operator"




--

main :: Effect Unit
main = do
  ref <- liftEffect Env.initEnvRef
  flip Env.runMalEnv ref do
    setArithOp
    loop