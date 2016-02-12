module VisualizationData.Queue.Interface where

import Data.Maybe (isNothing)

class Queue q where
  qempty :: q a
  qsnoc :: q a -> a -> q a
  quncons :: q a -> Maybe (a, q a)
  quncons q = (,) <$> qhead q <*> qtail q
  qhead :: q a -> Maybe a
  qhead = fmap fst . quncons
  qtail :: q a -> Maybe (q a)
  qtail = fmap snd . quncons
  qnull :: q a -> Bool
  qnull = isNothing . quncons