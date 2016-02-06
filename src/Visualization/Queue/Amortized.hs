{-# LANGUAGE OverloadedStrings, TypeFamilies, DeriveGeneric, DeriveAnyClass #-}

module Visualization.Queue.Amortized where

import React.Flux
import Data.Maybe (fromMaybe)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Monoid ((<>))
import Data.IORef (readIORef)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (forM_)

import Visualization.Common
import qualified VisualizationData.Queue.Amortized as AQ

data AQueueVisState =
  AQueueVisState
  { nextInt :: Int
  , queue :: AQ.AQueue Int
  , pastStates :: [AQ.AQueue Int]
  } deriving (Typeable)

initialState :: AQueueVisState
initialState = AQueueVisState 2 (AQ.fromList [1]) []

data AQueueAction
  = Tail
  | Snoc
  | Back
  | Clear
  deriving (Show, Eq, Typeable, Generic, NFData)

instance StoreData AQueueVisState where
  type StoreAction AQueueVisState = AQueueAction
  transform Tail (AQueueVisState k q hist) =
    pure $ AQueueVisState k (fromMaybe q (AQ.tail q)) (q:hist)
  transform Snoc (AQueueVisState k q hist) =
    pure $ AQueueVisState (k+1) (AQ.snoc q k) (q:hist)
  transform Back (AQueueVisState k q hist) =
    return $ case hist of
      [] -> AQueueVisState k q []
      (r:hist') -> AQueueVisState k r hist'
  transform Clear _ = return initialState
    

queueStore :: ReactStore AQueueVisState
queueStore = mkStore initialState

dispatchBQueueAction :: AQueueAction -> [SomeStoreAction]
dispatchBQueueAction a = [SomeStoreAction queueStore a]

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

-- Visualization of an amortized queue
aQueueVis :: ReactView ()
aQueueVis =
  defineControllerView "rtqueue-visualization" queueStore $ \(AQueueVisState k aq hist) _ ->
    div_ $ do
      p_ [ "className" $= "controls" ] $ do
        button_ [ "className" $= "pure-button back-button", "disabled" @= null hist, onClick (\_ _ -> dispatchBQueueAction Back) ] "back"
        " "
        button_ [ "className" $= "pure-button clear-button", onClick (\_ _ -> dispatchBQueueAction Clear) ] "clear"
        " "
        button_ [ "className" $= "pure-button tail-button", "disabled" @= AQ.null aq, onClick (\_ _ -> dispatchBQueueAction Tail) ] "tail(queue)"
        " "
        button_ [ "className" $= "pure-button snoc-button", onClick (\_ _ -> dispatchBQueueAction Snoc) ] $
          "snoc(queue, " <> elemShow k <> ")"
      div_ $ renderAQueue aq
      forM_ hist $ div_ . renderAQueue
  where
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