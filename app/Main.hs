module Main where

import BankersQueueVis
import RealTimeQueueVis
import AmoQueueVis
import React.Flux

main :: IO ()
main = do
  reactRender "bqueue-vis" bQueueVis ()
  reactRender "rtqueue-vis" rtQueueVis ()
  reactRender "aqueue-vis" aQueueVis ()