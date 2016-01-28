
{-# LANGUAGE OverloadedStrings, TypeFamilies, DeriveGeneric, DeriveAnyClass #-}

module RealTimeQueueVis where

--import GHCJS.Types
import React.Flux
import qualified RealTimeQueue as RTQ
import qualified LenList as LL
import Data.Maybe (fromMaybe)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Monoid ((<>))
import Control.Monad (forM_)
import Data.IORef (readIORef)
import System.IO.Unsafe (unsafePerformIO)

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

-- TODO: copied from BankersQueue visualization, make code more DRY
renderLenList :: LL.LenList Int -> ReactElementM handler ()
renderLenList (LL.LenList len items) =
  div_ [ "className" $= "len-list" ] $ do
    span_ [ "className" $= "len-list-length" ] $ "(length: " <> elemShow len <> ")"
    forM_ items $ \item ->
      span_ [ "className" $= "list-cell" ] $
        span_ [ "className" $= "item" ] (elemShow item)

renderLazyList :: RTQ.LazyListRef Int -> ReactElementM handler ()
renderLazyList ref =
  case unsafeRead ref of
    Left (RTQ.AppendReverseThunk xs rs ys) ->
      div_ [ "className" $= "thunk" ] $ do
        div_ $ renderLazyList xs
        "++ reverse"
        div_ $ forM_ ys $ \item ->
          span_ [ "className" $= "list-cell" ] $
            span_ [ "className" $= "item" ] (elemShow item)
        "++"
        div_ $ renderLazyList (RTQ.toRef rs)
    Right RTQ.Nil -> mempty
    Right (RTQ.Cons x xs) -> do
      span_ [ "className" $= "list-cell" ] $
        span_ [ "className" $= "item" ] (elemShow x)
      renderLazyList xs
  where unsafeRead = unsafePerformIO . readIORef

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
        renderLenList (LL.LenList rearL rear)