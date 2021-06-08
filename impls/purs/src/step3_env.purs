module Main where

import Prelude

import Control.Monad.Reader.Class (ask)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Env (MalEnv, get)
import Env as Env
import Mal.Reader (readStr)
import Printer (printStr)
import Readline (readLine)
import Types (MalExpr(..), MalFn)



-- READ

read :: String -> Either String MalExpr
read = readStr



-- EVAL

-- FIXME: let*
eval :: MalExpr -> MalEnv MalExpr
eval ast@(MalList Nil)  = pure ast
eval (MalList ast)    = do
  case ast of
    ((MalSymbol "def!") : _)  -> evalDef ast
    ((MalSymbol "let*"): _) -> evalLet ast
    _                         -> do
      es <- traverse evalAst ast
      case es of
        (MalFunction {fn:f} : args) -> liftEffect $ f args
        _ -> liftEffect $ throw $ "no reachable ??"
eval ast              = evalAst ast


evalAst :: MalExpr -> MalEnv MalExpr
evalAst (MalSymbol s)   = do
  ref <- ask
  es <- liftEffect $ Ref.read ref
  case get s es of
    Just k -> pure k
    Nothing -> liftEffect $ throw $ s <> " is not found"
evalAst ast@(MalList _) = eval ast
evalAst (MalVector es)  = MalVector <$> (traverse eval es)
evalAst (MalHashMap es) = MalHashMap <$> (traverse eval es)
evalAst ast             = pure ast


-- FIXME: 関数わける必要ないかも
evalDef :: List MalExpr -> MalEnv MalExpr
evalDef ((MalSymbol "def!") : Nil) = liftEffect $ throw "invalid def!"
evalDef ((MalSymbol "def!") : (MalSymbol v) : e : Nil) = do
  evd <- evalAst e
  Env.set v evd
  pure evd
evalDef _ = liftEffect $ throw "no reachable"


-- FIXME: 関数わける必要ないかも
evalLet :: List MalExpr -> MalEnv MalExpr
evalLet ((MalSymbol "let*") : Nil) = liftEffect $ throw "invalid let*"
evalLet ((MalSymbol "let*") : (MalList ps) : e : Nil) = do
  -- FIXME: 新しいEnvにセットしないといけない
  Env.newEnv
  _ <- letBind ps
  eval e
evalLet _ = liftEffect $ throw "no reachable"


letBind :: List MalExpr -> MalEnv Unit
letBind Nil = pure unit
letBind (MalSymbol b : e : xs) = do
  ev <- eval e
  Env.set b ev
  letBind xs
letBind _ = liftEffect $ throw "invalid let* 3"



-- PRINT

print :: MalExpr -> String
print = printStr



-- REPL

rep :: String -> MalEnv String
rep str = case read str of
  Left _ -> liftEffect $ throw "EOF"
  Right ast -> do
    result <- eval ast
    pure $ print result


loop :: MalEnv Unit
loop = do
  line <- liftEffect readLine
  case line of
    ":q" -> pure unit
    _    -> do
      -- FIXME: try
      result <- rep line
      -- result <- try $ rep line
      -- case result of
      --   Right exp -> liftEffect $ log exp
      --   Left err  -> liftEffect $ error $ show err
      liftEffect $ log result
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
  ref <- liftEffect Env.make
  flip Env.runMalEnv ref do
    setArithOp
    loop