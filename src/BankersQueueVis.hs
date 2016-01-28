{-# LANGUAGE OverloadedStrings, TypeFamilies, DeriveGeneric, DeriveAnyClass #-}

module BankersQueueVis where

--import GHCJS.Types
import React.Flux
import qualified BankersQueue as BQ
import qualified LenList as LL
import Data.Maybe (fromMaybe)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Monoid ((<>))
import Control.Monad (forM_)

data BQueueVisState =
  BQueueVisState
  { nextInt :: Int
  , queue :: BQ.BQueue Int
  , pastStates :: [BQ.BQueue Int]
  } deriving (Eq, Show, Typeable)

data BQueueAction
  = Tail
  | Snoc
  | Back
  deriving (Show, Eq, Typeable, Generic, NFData)

instance StoreData BQueueVisState where
  type StoreAction BQueueVisState = BQueueAction
  transform Tail (BQueueVisState k q hist) =
    pure $ BQueueVisState k (fromMaybe q (BQ.tail q)) (q:hist)
  transform Snoc (BQueueVisState k q hist) =
    pure $ BQueueVisState (k+1) (BQ.snoc q k) (q:hist)
  transform Back (BQueueVisState k q hist) =
    return $ case hist of
      [] -> BQueueVisState k q []
      (r:hist') -> BQueueVisState k r hist'

queueStore :: ReactStore BQueueVisState
queueStore = mkStore $ BQueueVisState 2 (BQ.fromList [1]) []

dispatchBQueueAction :: BQueueAction -> [SomeStoreAction]
dispatchBQueueAction a = [SomeStoreAction queueStore a]

renderLenList :: LL.LenList Int -> ReactElementM handler ()
renderLenList (LL.LenList len items) =
  div_ [ "className" $= "len-list" ] $ do
    span_ [ "className" $= "len-list-length" ] $ "(length: " <> elemShow len <> ")"
    forM_ items $ \item ->
      span_ [ "className" $= "list-cell" ] $
        span_ [ "className" $= "item" ] (elemShow item)

-- Visualization of a banker's queue
bQueueVis :: ReactView ()
bQueueVis =
  defineControllerView "bqueue-visualization" queueStore $ \(BQueueVisState k bq@(BQ.BQueue xs ys) hist) _ ->
    div_ $ do
      p_ [ "className" $= "controls" ] $ do
        button_ [ "className" $= "pure-button back-button", "disabled" @= null hist, onClick (\_ _ -> dispatchBQueueAction Back) ] "back"
        " "
        button_ [ "className" $= "pure-button tail-button", "disabled" @= BQ.null bq, onClick (\_ _ -> dispatchBQueueAction Tail) ] "tail(queue)"
        " "
        button_ [ "className" $= "pure-button snoc-button", onClick (\_ _ -> dispatchBQueueAction Snoc) ] $
          "snoc(queue, " <> elemShow k <> ")"
      div_ [ "className" $= "front" ] $ do
        span_ [ "className" $= "len-list-name" ] "front"
        renderLenList xs
      div_ [ "className" $= "rear" ] $ do
        span_ [ "className" $= "len-list-name" ] "rear"
        renderLenList ys
