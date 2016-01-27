{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( helloWorld
    ) where

--import GHCJS.Types
import React.Flux

helloWorld :: ReactView ()
helloWorld =
  defineView "hello-world" $ const $
    p_ "Hello World!"