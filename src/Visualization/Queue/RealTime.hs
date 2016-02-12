{-# LANGUAGE OverloadedStrings, TypeFamilies, DeriveGeneric, DeriveAnyClass, PatternSynonyms #-}

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

pattern RTQVS nextInt queue hist = RTQueueVisState (VQG.QueueVisState nextInt queue hist)

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
  transform (SimpleQueueAction VQG.Tail) (RTQVS k q hist) = do
    forceStepFuture
    pure $ RTQVS k (fromMaybe q (RTQ.tail q)) (q:hist)
  transform (SimpleQueueAction VQG.Snoc) (RTQVS k q hist) = do
    forceStepFuture
    pure $ RTQVS (k+1) (RTQ.snoc q k) (q:hist)
  transform (SimpleQueueAction action) (RTQueueVisState state) =
    RTQueueVisState <$> transform action state
  transform ScheduleStep (RTQVS k q hist) =
    return $ RTQVS k (RTQ.schedStep q) hist

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

renderLazyList :: Show a => RTQ.LazyListRef a -> ReactElementM handler ()
renderLazyList = go True
  where
    go isToplevel thunk =
      case readThunk thunk of
        Left (RTQ.AppendReverseThunk xs rs ys) ->
          cldiv_ "list thunk" $ do
            cldiv_ "list" $ go True xs
            code_ " ++ reverse "
            renderList ys
            code_ " ++ "
            go True (wrapThunk rs)
        Right RTQ.Nil ->
          if isToplevel then cldiv_ "list empty" mempty else mempty
        Right (RTQ.Cons x xs) ->
          (if isToplevel then cldiv_ "list" else id) $ do
            clspan_ "list-cell" $ clspan_ "item" (elemShow x)
            go False xs

renderRTQueue :: Show a => RTQ.RTQueue a -> ReactElementM handler ()
renderRTQueue (RTQ.RTQueue front frontL rear rearL _) = do
  div_ [ "className" $= "front" ] $ do
    span_ [ "className" $= "len-list-name" ] "front"
    div_ [ "className" $= "len-list" ] $ do
      span_ [ "className" $= "len-list-length" ] $ "(length: " <> elemShow frontL <> ")"
      renderLazyList front
  div_ [ "className" $= "rear" ] $ do
    span_ [ "className" $= "len-list-name" ] "rear"
    renderListWithLen rear rearL