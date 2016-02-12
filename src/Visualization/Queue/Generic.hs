{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Visualization.Queue.Generic
  ( QueueVisState (..)
  , initialState
  , QueueAction (..)
  , renderControls
  , defineQueueVis
  ) where

import VisualizationData.Queue.Interface

import React.Flux
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Control.Monad (forM_)

data QueueVisState q =
  QueueVisState
  { nextInt :: Int
  , queue :: q Int
  , pastStates :: [q Int]
  } deriving (Typeable)

deriving instance Show (q Int) => Show (QueueVisState q)

initialState :: Queue q => QueueVisState q
initialState = QueueVisState 2 (qsnoc qempty 1) []

data QueueAction
  = Tail
  | Snoc
  | Back
  | Clear
  deriving (Show, Eq, Typeable, Generic, NFData)

instance (Queue q, Typeable q) => StoreData (QueueVisState q) where
  type StoreAction (QueueVisState q) = QueueAction
  transform Tail (QueueVisState k q hist) =
    pure $ QueueVisState k (fromMaybe q (qtail q)) (q:hist)
  transform Snoc (QueueVisState k q hist) =
    pure $ QueueVisState (k+1) (qsnoc q k) (q:hist)
  transform Back (QueueVisState k q hist) =
    return $ case hist of
      [] -> QueueVisState k q []
      (r:hist') -> QueueVisState k r hist'
  transform Clear _ = return initialState

renderControls
  :: Queue q
  => QueueVisState q
  -> (QueueAction -> [SomeStoreAction])
  -> ReactElementM ViewEventHandler ()
renderControls (QueueVisState k bq hist) dispatch =
  p_ [ "className" $= "controls" ] $ do
    button_ [ "className" $= "pure-button back-button", "disabled" @= null hist, onClick (\_ _ -> dispatch Back) ] "back"
    " "
    button_ [ "className" $= "pure-button clear-button", onClick (\_ _ -> dispatch Clear) ] "clear"
    " "
    button_ [ "className" $= "pure-button tail-button", "disabled" @= qnull bq, onClick (\_ _ -> dispatch Tail) ] "tail(queue)"
    " "
    button_ [ "className" $= "pure-button snoc-button", onClick (\_ _ -> dispatch Snoc) ] $
      "snoc(queue, " <> elemShow k <> ")"

defineQueueVis
  :: (Queue q, Typeable q)
  => String
  -> (q Int -> ReactElementM ViewEventHandler ())
  -> ReactView ()
defineQueueVis name (renderQueue :: q Int -> ReactElementM ViewEventHandler ()) =
  defineControllerView name queueStore $ \qvs@(QueueVisState _ queue hist) _ ->
    div_ $ do
      renderControls qvs dispatch
      div_ $ renderQueue queue
      forM_ hist $ div_ . renderQueue
  where
    queueStore :: ReactStore (QueueVisState q)
    queueStore = mkStore initialState
    dispatch :: QueueAction -> [SomeStoreAction]
    dispatch a = [SomeStoreAction queueStore a]
