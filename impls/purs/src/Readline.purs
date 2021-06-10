module Readline where

import Effect (Effect)

foreign import readLine :: Effect String
