module Readline where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, Canceler, effectCanceler, makeAff)
import Effect.Exception (Error)
import Node.ReadLine (close, createConsoleInterface, noCompletion, prompt, setLineHandler, setPrompt)

readLine :: String -> Aff String
readLine pm = makeAff handler
  where
  handler :: (Either Error String -> Effect Unit) -> Effect Canceler
  handler next = do
    interface <- createConsoleInterface noCompletion
    setPrompt pm interface
    prompt interface
    interface
      # setLineHandler \str -> do
          close interface
          next $ Right str
    pure $ effectCanceler $ close interface
