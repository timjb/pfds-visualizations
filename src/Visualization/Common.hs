{-# LANGUAGE OverloadedStrings #-}

module Visualization.Common where

import Control.Monad (zipWithM_)
import Data.JSString (JSString)
import Data.Monoid ((<>))
import React.Flux

renderList :: Show a => [a] -> ReactElementM handler ()
renderList xs =
  if null xs then
    cldiv_ "list empty" " "
  else
    cldiv_ "list" $
      forZipWithM_ xs ([0..] :: [Int]) $ \x i ->
        span_ [ "key" &= i, "className" $= "list-cell" ] $
          clspan_ "item" (elemShow x)
  where
    forZipWithM_ as bs f = zipWithM_ f as bs

renderListWithLen :: Show a => [a] -> Int -> ReactElementM handler ()
renderListWithLen xs len =
  cldiv_ "len-list" $ do
    clspan_ "len-list-length" $
      "(length: " <> elemShow len <> ")"
    renderList xs

clspan_ :: JSString -> ReactElementM handler a -> ReactElementM handler a
clspan_ cl =
  span_ [ "className" $= cl ]