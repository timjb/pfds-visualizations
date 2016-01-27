module Main where

import Lib
import React.Flux

main :: IO ()
main = reactRender "app" helloWorld ()