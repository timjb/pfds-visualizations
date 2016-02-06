{-# LANGUAGE OverloadedStrings, TypeFamilies, DeriveGeneric, DeriveAnyClass #-}

module Visualization.Queue.RealTime where

import React.Flux
import Data.Maybe (fromMaybe)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Monoid ((<>))
import Data.IORef (readIORef)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (forM_, void)
import Control.Concurrent (threadDelay, forkIO)

import Visualization.Common
import qualified VisualizationData.Queue.RealTime as RTQ

data RTQueueVisState =
  RTQueueVisState
  { nextInt :: Int
  , queue :: RTQ.RTQueue Int
  , pastStates :: [RTQ.RTQueue Int]
  } deriving (Typeable)

initialState :: RTQueueVisState
initialState = RTQueueVisState 2 (RTQ.fromList [1]) []

data RTQueueAction
  = Tail
  | Snoc
  | Back
  | Clear
  | ScheduleStep
  deriving (Show, Eq, Typeable, Generic, NFData)

forceStepFuture :: IO ()
forceStepFuture =
  void $ forkIO $ do
    threadDelay (700*1000)
    alterStore queueStore ScheduleStep

instance StoreData RTQueueVisState where
  type StoreAction RTQueueVisState = RTQueueAction
  transform Tail (RTQueueVisState k q hist) = do
    forceStepFuture
    pure $ RTQueueVisState k (fromMaybe q (RTQ.tail q)) (q:hist)
  transform Snoc (RTQueueVisState k q hist) = do
    forceStepFuture
    pure $ RTQueueVisState (k+1) (RTQ.snoc q k) (q:hist)
  transform Back (RTQueueVisState k q hist) =
    return $ case hist of
      [] -> RTQueueVisState k q []
      (r:hist') -> RTQueueVisState k r hist'
  transform Clear _ = return initialState
  transform ScheduleStep (RTQueueVisState k q hist) =
    return $ RTQueueVisState k (RTQ.schedStep q) hist

queueStore :: ReactStore RTQueueVisState
queueStore = mkStore initialState

dispatchBQueueAction :: RTQueueAction -> [SomeStoreAction]
dispatchBQueueAction a = [SomeStoreAction queueStore a]

renderLazyList :: Show a => RTQ.LazyListRef a -> ReactElementM handler ()
renderLazyList = go True
  where
    unsafeRead = unsafePerformIO . readIORef
    go isToplevel ref =
      case unsafeRead ref of
        Left (RTQ.AppendReverseThunk xs rs ys) ->
          cldiv_ "list thunk" $ do
            cldiv_ "list" $ go True xs
            code_ " ++ reverse "
            renderList ys
            code_ " ++ "
            go True (RTQ.toRef rs)
        Right RTQ.Nil ->
          if isToplevel then cldiv_ "list empty" mempty else mempty
        Right (RTQ.Cons x xs) ->
          (if isToplevel then cldiv_ "list" else id) $ do
            clspan_ "list-cell" $ clspan_ "item" (elemShow x)
            go False xs

-- Visualization of a Real-Time-queue
rtQueueVis :: ReactView ()
rtQueueVis =
  defineControllerView "rtqueue-visualization" queueStore $ \(RTQueueVisState k rtq hist) _ ->
    div_ $ do
      p_ [ "className" $= "controls" ] $ do
        button_ [ "className" $= "pure-button back-button", "disabled" @= null hist, onClick (\_ _ -> dispatchBQueueAction Back) ] "back"
        " "
        button_ [ "className" $= "pure-button clear-button", onClick (\_ _ -> dispatchBQueueAction Clear) ] "clear"
        " "
        button_ [ "className" $= "pure-button tail-button", "disabled" @= RTQ.null rtq, onClick (\_ _ -> dispatchBQueueAction Tail) ] "tail(queue)"
        " "
        button_ [ "className" $= "pure-button snoc-button", onClick (\_ _ -> dispatchBQueueAction Snoc) ] $
          "snoc(queue, " <> elemShow k <> ")"
      div_ $ renderRTQueue rtq
      forM_ hist $ div_ . renderRTQueue
  where
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