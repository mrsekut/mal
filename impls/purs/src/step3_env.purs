module Main where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (error, log)
import Effect.Exception (throw, try)
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

-- FIXME: error message
-- FIXME: let*
eval :: MalExpr -> MalEnv MalExpr
eval ast@(MalList Nil)  = pure ast
eval (MalList ast)    = do
  es <- traverse evalAst ast        -- es::List MalExpr
  case es of
    (MalFunction {fn:f}: args) -> liftEffect $ f args
    -- ((MalSymbol "def!"): Nil) -> liftEffect $ throw "invalid function"
    -- ((MalSymbol "def!"): (MalSymbol a) : e : Nil) -> do
    --   evd <- evalAst e
    --   Env.set a evd
    --   pure evd
    -- ((MalSymbol "let*"): Nil) -> liftEffect $ throw "invalid function"
    _                          -> pure $ MalList es
eval ast              = evalAst ast


evalAst :: MalExpr -> MalEnv MalExpr
evalAst (MalSymbol s)   = Env.get s
evalAst ast@(MalList _) = eval ast
evalAst (MalVector es)  = MalVector <$> (traverse eval es)
evalAst (MalHashMap es) = MalHashMap <$> (traverse eval es)
evalAst ast             = pure ast



-- PRINT

print :: MalExpr -> String
print = printStr



-- REPL

rep :: String -> MalEnv String
rep str = do
  setArithOp
  case read str of
    Left _ -> liftEffect $ throw "EOF"
    Right ast -> do
      result <- eval ast
      pure $ print result

loop :: Aff Unit
loop = do
  line <- readLine "user> "
  case line of
    ":q" -> pure unit
    ":Q" -> pure unit
    _    -> do
      ref <- liftEffect Env.make
      result <- liftEffect $ try $ Env.runMalEnv (rep line) ref
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
main = launchAff_ loop

