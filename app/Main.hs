module Main where

import React.Flux

import Visualization.Queue.Bankers
import Visualization.Queue.Amortized
import Visualization.Queue.RealTime

main :: IO ()
main = do
  reactRender "bqueue-vis" bQueueVis ()
  reactRender "rtqueue-vis" rtQueueVis ()
  reactRender "aqueue-vis" aQueueVis ()