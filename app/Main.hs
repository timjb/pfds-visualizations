module Main where

import BankersQueueVis
import RealTimeQueueVis
import React.Flux

main :: IO ()
main = do
  reactRender "bqueue-vis" bQueueVis ()
  reactRender "rtqueue-vis" rtQueueVis ()