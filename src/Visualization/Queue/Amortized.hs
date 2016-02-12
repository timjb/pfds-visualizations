{-# LANGUAGE OverloadedStrings, TypeFamilies, DeriveGeneric, DeriveAnyClass #-}

module Visualization.Queue.Amortized (aQueueVis) where

import Visualization.Common
import qualified VisualizationData.Queue.Amortized as AQ
import Visualization.Queue.Generic

import React.Flux
import Data.Monoid ((<>))
-- TODO: Move somewhere else
import Data.IORef (readIORef)
import System.IO.Unsafe (unsafePerformIO)

aQueueVis :: ReactView ()
aQueueVis = defineQueueVis "amoqueue-visualization" renderAQueue

renderAQueue :: Show a => AQ.AQueue a -> ReactElementM handler ()
renderAQueue (AQ.AQueue front frontL rear rearL) = do
  div_ [ "className" $= "front" ] $ do
    span_ [ "className" $= "len-list-name" ] "front"
    div_ [ "className" $= "len-list" ] $ do
      span_ [ "className" $= "len-list-length" ] $ "(length: " <> elemShow frontL <> ")"
      renderLazyList front
  div_ [ "className" $= "rear" ] $ do
    span_ [ "className" $= "len-list-name" ] "rear"
    renderListWithLen rear rearL

renderLazyList :: Show a => AQ.LazyListRef a -> ReactElementM handler ()
renderLazyList = go True
  where
    unsafeRead = unsafePerformIO . readIORef
    go isToplevel ref =
      case unsafeRead ref of
        Left (AQ.AppendThenReverseThunk xs ys) ->
          cldiv_ "list thunk" $ do
            cldiv_ "list" $ go True xs
            code_ " ++ reverse "
            renderList ys
        Right AQ.Nil ->
          if isToplevel then cldiv_ "list empty" mempty else mempty
        Right (AQ.Cons x xs) ->
          (if isToplevel then cldiv_ "list" else id) $ do
            clspan_ "list-cell" $ clspan_ "item" (elemShow x)
            go False xs