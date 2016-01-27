module Main where

import Lib
import React.Flux
--import qualified BankersQueue as BQ

main :: IO ()
main = reactRender "app" bQueueVis ()