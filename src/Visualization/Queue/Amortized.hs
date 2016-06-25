{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Visualization.Queue.Amortized (aQueueVis) where

import Visualization.Common
import Visualization.Queue.Generic
import VisualizationData.Thunk
import qualified VisualizationData.Queue.Amortized as AQ

import Data.Monoid ((<>))
import React.Flux

aQueueVis :: ReactView ()
aQueueVis = defineQueueVis "amoqueue-visualization" renderAQueue

renderAQueue :: Show a => AQ.AQueue a -> ReactElementM handler ()
renderAQueue (AQ.AQueue front frontL rear rearL) = do
  cldiv_ "front" $ do
    clspan_ "len-list-name" "front"
    cldiv_ "len-list" $ do
      clspan_ "len-list-length" $
        "(length: " <> elemShow frontL <> ")"
      renderLazyList front
  cldiv_ "rear" $ do
    clspan_ "len-list-name" "rear"
    renderListWithLen rear rearL

renderLazyList :: Show a => AQ.LazyListRef a -> ReactElementM handler ()
renderLazyList =
  go True
  where
    go isToplevel thunk =
      case readThunk thunk of
        Left (AQ.AppendThenReverseThunk xs ys) ->
          cldiv_ "list thunk" $ do
            cldiv_ "list" $ go True xs
            code_ " ++ reverse "
            renderList ys
        Right AQ.Nil ->
          if isToplevel then
            cldiv_ "list empty" mempty
          else
            mempty
        Right (AQ.Cons x xs) ->
          (if isToplevel then cldiv_ "list" else id) $ do
            clspan_ "list-cell" $ clspan_ "item" (elemShow x)
            go False xs