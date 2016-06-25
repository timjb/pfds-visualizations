{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Visualization.Queue.Bankers (bQueueVis) where

import Visualization.Common
import Visualization.Queue.Generic
import qualified VisualizationData.LenList as LL
import qualified VisualizationData.Queue.Bankers as BQ

import React.Flux


bQueueVis :: ReactView ()
bQueueVis =
  defineQueueVis "bqueue-visualization" renderBQueue

renderBQueue :: BQ.BQueue Int -> ReactElementM handler ()
renderBQueue (BQ.BQueue xs ys) = do
  cldiv_ "front" $ do
    clspan_ "len-list-name" "front"
    renderLenList xs
  cldiv_ "rear" $ do
    clspan_ "len-list-name" "rear"
    renderLenList ys

renderLenList :: LL.LenList Int -> ReactElementM handler ()
renderLenList (LL.LenList len items) =
  renderListWithLen items len
