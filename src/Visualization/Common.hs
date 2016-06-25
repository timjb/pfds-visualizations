{-# LANGUAGE OverloadedStrings #-}

module Visualization.Common where

import React.Flux
import Control.Monad (forM_)
import Data.Monoid ((<>))

renderList :: Show a => [a] -> ReactElementM handler ()
renderList xs =
  if null xs then
    cldiv_ "list empty" " "
  else
    cldiv_ "list" $
      forM_ xs $ \x ->
        clspan_ "list-cell" $
          clspan_ "item" (elemShow x)

renderListWithLen :: Show a => [a] -> Int -> ReactElementM handler ()
renderListWithLen xs len =
  cldiv_ "len-list" $ do
    clspan_ "len-list-length" $
      "(length: " <> elemShow len <> ")"
    renderList xs

clspan_ :: String -> ReactElementM handler a -> ReactElementM handler a
clspan_ cl =
  span_ [ "className" @= cl ]