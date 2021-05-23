module Mal.Step0repl where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Readline (readLine)

main :: Effect Unit
main = do
  launchAff_ loop

loop :: Aff Unit
loop = do
  line <- readLine "user> "
  case line of
    ":q" -> pure unit
    ":Q" -> pure unit
    _ -> do
      liftEffect $ log line
      loop

read :: String -> String
read s = s

eval :: String -> String
eval s = s

print :: String -> String
print s = s

rep :: String -> String
rep = read >>> eval >>> print
