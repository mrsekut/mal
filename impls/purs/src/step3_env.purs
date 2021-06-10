module Main3 where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Reader.Class (ask)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (error, log)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Env (MalEnv)
import Env as Env
import Mal.Reader (readStr)
import Printer (printStr)
import Readline (readLine)
import Types (MalExpr(..), MalFn)



-- READ

read :: String -> Either String MalExpr
read = readStr



-- EVAL

eval :: MalExpr -> MalEnv MalExpr
eval ast@(MalList Nil) = pure ast
eval (MalList ast)     = case ast of
  ((MalSymbol "def!") : _)  -> evalDef ast
  ((MalSymbol "let*") : _)  -> evalLet ast
  _                         -> do
    es <- traverse evalAst ast
    case es of
      (MalFunction {fn:f} : args) -> liftEffect $ f args
      _                           -> liftEffect $ throw $ "invalid function"
eval ast               = evalAst ast


evalAst :: MalExpr -> MalEnv MalExpr
evalAst (MalSymbol s)   = do
  ref <- ask
  envs <- liftEffect $ Ref.read ref
  case Env.get s envs of
    Just k  -> pure k
    Nothing -> liftEffect $ throw $ "'" <> s <> "'" <> " not found"
evalAst ast@(MalList _)   = eval ast
evalAst (MalVector envs)  = MalVector <$> traverse eval envs
evalAst (MalHashMap envs) = MalHashMap <$> traverse eval envs
evalAst ast               = pure ast


evalDef :: List MalExpr -> MalEnv MalExpr
evalDef ((MalSymbol "def!") : Nil)                     = liftEffect $ throw "invalid def!"
evalDef ((MalSymbol "def!") : (MalSymbol v) : e : Nil) = do
  evd <- evalAst e
  Env.set v evd
  pure evd
evalDef _                                              = liftEffect $ throw "invalid def!"


evalLet :: List MalExpr -> MalEnv MalExpr
evalLet ((MalSymbol "let*") : Nil)                      = liftEffect $ throw "invalid let*"
evalLet ((MalSymbol "let*") : (MalList ps) : e : Nil)   = Env.local $ letBind ps *> eval e
evalLet ((MalSymbol "let*") : (MalVector ps) : e : Nil) = Env.local $ letBind ps *> eval e
evalLet _                                               = liftEffect $ throw "invalid let*"


letBind :: List MalExpr -> MalEnv Unit
letBind Nil                     = pure unit
letBind (MalSymbol ky : e : es) = (Env.set ky =<< eval e) *> letBind es
letBind _                       = liftEffect $ throw "invalid let*"



-- PRINT

print :: MalExpr -> String
print = printStr



-- REPL

rep :: String -> MalEnv String
rep str = case read str of
  Left _    -> liftEffect $ throw "EOF"
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


fn :: (Int -> Int -> Int) -> MalExpr
fn op = MalFunction $ { fn : g op }
  where
  g :: (Int -> Int -> Int) -> MalFn
  g op' ((MalInt n1) : (MalInt n2) : Nil) = pure $ MalInt $ op' n1 n2
  g _ _                                   = throw "invalid operator"



--

main :: Effect Unit
main = do
  ref <- liftEffect Env.initEnvRef
  flip Env.runMalEnv ref do
    setArithOp
    loop