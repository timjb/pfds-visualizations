{-# LANGUAGE OverloadedStrings, TypeFamilies, DeriveGeneric, DeriveAnyClass, PatternSynonyms, ViewPatterns #-}

module Visualization.Queue.RealTime (rtQueueVis) where

import Visualization.Common
import qualified VisualizationData.Queue.RealTime as RTQ
import qualified Visualization.Queue.Generic as VQG
import VisualizationData.Thunk

import React.Flux
import Data.Maybe (fromMaybe)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Monoid ((<>))
import Control.Monad (forM_, void)
import Control.Concurrent (threadDelay, forkIO)

newtype RTQueueVisState = RTQueueVisState (VQG.QueueVisState RTQ.RTQueue)
  deriving (Typeable)

pattern RTQVS nextInt queue hist =
  RTQueueVisState (VQG.QueueVisState nextInt queue hist)

initialState :: RTQueueVisState
initialState = RTQueueVisState VQG.initialState

data RTQueueAction
  = SimpleQueueAction VQG.QueueAction
  | ScheduleStep
  deriving (Show, Eq, Typeable, Generic, NFData)

forceStepFuture :: IO ()
forceStepFuture =
  void $ forkIO $ do
    threadDelay (700*1000)
    alterStore queueStore ScheduleStep

instance StoreData RTQueueVisState where
  type StoreAction RTQueueVisState = RTQueueAction
  transform (SimpleQueueAction VQG.Tail) (RTQVS k (RTQ.schedStep -> q) hist) = do
    forceStepFuture
    pure $ RTQVS k (fromMaybe q (RTQ.tail' q)) (q:hist)
  transform (SimpleQueueAction VQG.Snoc) (RTQVS k (RTQ.schedStep -> q) hist) = do
    forceStepFuture
    pure $ RTQVS (k+1) (RTQ.snoc' q k) (q:hist)
  -- remaining two SimpleQueueAction's: back and clear
  transform (SimpleQueueAction action) (RTQueueVisState state) =
    RTQueueVisState <$> transform action state
  transform ScheduleStep (RTQVS k q hist)
    = pure (RTQVS k (RTQ.schedStep q) hist)

queueStore :: ReactStore RTQueueVisState
queueStore = mkStore initialState

dispatch :: RTQueueAction -> [SomeStoreAction]
dispatch a = [SomeStoreAction queueStore a]

rtQueueVis :: ReactView ()
rtQueueVis =
  defineControllerView "rtqueue-visualization" queueStore $
    \(RTQueueVisState queueState@(VQG.QueueVisState _ rtq hist)) _ ->
      div_ $ do
        VQG.renderControls queueState (dispatch . SimpleQueueAction)
        div_ $ renderRTQueue rtq
        forM_ hist $ div_ . renderRTQueue

renderLazyList :: Show a => RTQ.LazyListRef a -> Int -> ReactElementM handler ()
renderLazyList = go True
  where
    schedWrapper schedHtml =
      cldiv_ "list schedule" $ do
        clspan_ "len-list-name" "schedule"
        schedHtml
    go isToplevel thunk ix =
      (if ix == 0 || (isToplevel && ix == -1) then schedWrapper else id) $
      case readThunk thunk of
        Left (RTQ.AppendReverseThunk xs rs ys) ->
          cldiv_ "list thunk" $ do
            cldiv_ "list" $ go True xs (-10)
            code_ " ++ reverse "
            renderList ys
            code_ " ++ "
            go True (wrapThunk rs) (-10)
        Right RTQ.Nil ->
          if isToplevel then cldiv_ "list empty" mempty else mempty
        Right (RTQ.Cons x xs) ->
          (if isToplevel then cldiv_ "list" else id) $ do
            clspan_ "list-cell" $ clspan_ "item" (elemShow x)
            go False xs (ix-1)

renderRTQueue :: Show a => RTQ.RTQueue a -> ReactElementM handler ()
renderRTQueue queue@(RTQ.RTQueue front frontL rear rearL _ _) = do
  cldiv_ "front" $ do
    clspan_ "len-list-name" "front"
    cldiv_ "len-list" $ do
      clspan_ "len-list-length" $ "(length: " <> elemShow frontL <> ")"
      renderLazyList front (RTQ.scheduleIndex queue)
  cldiv_ "rear" $ do
    clspan_ "len-list-name" "rear"
    renderListWithLen rear rearL