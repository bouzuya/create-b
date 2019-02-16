module Main where

import Data.Array as Array
import Effect (Effect)
import Effect.Class.Console as Console
import Node.Process as Process
import Prelude (Unit, bind, map)

main :: Effect Unit
main = do
  args <- map (Array.drop 2) Process.argv
  Console.logShow args
