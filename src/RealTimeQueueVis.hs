{-# LANGUAGE OverloadedStrings, TypeFamilies, DeriveGeneric, DeriveAnyClass #-}

module RealTimeQueueVis where

import React.Flux
import Data.Maybe (fromMaybe)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Monoid ((<>))
import Data.IORef (readIORef)
import System.IO.Unsafe (unsafePerformIO)

import ReactCommon
import qualified RealTimeQueue as RTQ

data RTQueueVisState =
  RTQueueVisState
  { nextInt :: Int
  , queue :: RTQ.RTQueue Int
  , pastStates :: [RTQ.RTQueue Int]
  } deriving (Typeable)

data RTQueueAction
  = Tail
  | Snoc
  | Back
  deriving (Show, Eq, Typeable, Generic, NFData)

instance StoreData RTQueueVisState where
  type StoreAction RTQueueVisState = RTQueueAction
  transform Tail (RTQueueVisState k q hist) =
    pure $ RTQueueVisState k (fromMaybe q (RTQ.tail q)) (q:hist)
  transform Snoc (RTQueueVisState k q hist) =
    pure $ RTQueueVisState (k+1) (RTQ.snoc q k) (q:hist)
  transform Back (RTQueueVisState k q hist) =
    return $ case hist of
      [] -> RTQueueVisState k q []
      (r:hist') -> RTQueueVisState k r hist'

queueStore :: ReactStore RTQueueVisState
queueStore = mkStore $ RTQueueVisState 2 (RTQ.fromList [1]) []

dispatchBQueueAction :: RTQueueAction -> [SomeStoreAction]
dispatchBQueueAction a = [SomeStoreAction queueStore a]

renderLazyList :: RTQ.LazyListRef Int -> ReactElementM handler ()
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

-- Visualization of a banker's queue
rtQueueVis :: ReactView ()
rtQueueVis =
  defineControllerView "rtqueue-visualization" queueStore $ \(RTQueueVisState k bq@(RTQ.RTQueue front frontL rear rearL) hist) _ ->
    div_ $ do
      p_ [ "className" $= "controls" ] $ do
        button_ [ "className" $= "pure-button back-button", "disabled" @= null hist, onClick (\_ _ -> dispatchBQueueAction Back) ] "back"
        " "
        button_ [ "className" $= "pure-button tail-button", "disabled" @= RTQ.null bq, onClick (\_ _ -> dispatchBQueueAction Tail) ] "tail(queue)"
        " "
        button_ [ "className" $= "pure-button snoc-button", onClick (\_ _ -> dispatchBQueueAction Snoc) ] $
          "snoc(queue, " <> elemShow k <> ")"
      div_ [ "className" $= "front" ] $ do
        span_ [ "className" $= "len-list-name" ] "front"
        div_ [ "className" $= "len-list" ] $ do
          span_ [ "className" $= "len-list-length" ] $ "(length: " <> elemShow frontL <> ")"
          renderLazyList front
      div_ [ "className" $= "rear" ] $ do
        span_ [ "className" $= "len-list-name" ] "rear"
        renderListWithLen rear rearL