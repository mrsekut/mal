module Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (error, log)
import Mal.Reader (readStr)
import Printer (printStr)
import Readline (readLine)
import Types (MalExpr)


-- READ

read :: String -> Either String MalExpr
read = readStr



-- EVAL

eval :: String -> String
eval s = s



-- PRINT

print :: String -> String
print s = s



-- REPL

rep :: String -> Effect Unit
rep str = case read str of
  Left _ -> error "EOF"
  Right s -> log $ printStr s # eval # print

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
