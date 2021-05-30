module Main where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Mal.Reader (readString)
import Printer (printStr)
import Readline (readLine)
import Types (MalExpr)

main :: Effect Unit
main = do
  launchAff_ loop

loop :: Aff Unit
loop = do
  line <- readLine "user> "
  case line of
    ":q" -> pure unit
    ":Q" -> pure unit
    str -> do
      liftEffect $ log $ rep line
      loop

read :: String -> Either String MalExpr
read = readString

eval :: String -> String
eval s = s

print :: String -> String
print s = s

rep :: String -> String
rep str = case read str of
  Left s -> "aa"
  Right s -> printStr s # eval # print
