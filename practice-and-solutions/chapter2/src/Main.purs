module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log, logShow)
import Math (sqrt)

diagonal w h = sqrt (w*w+h*h)

main :: Effect Unit
main = do
  logShow (diagonal 3.0 4.0)
