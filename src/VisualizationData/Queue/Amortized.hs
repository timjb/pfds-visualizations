{-# LANGUAGE LambdaCase #-}

module VisualizationData.Queue.Amortized where

import VisualizationData.Queue.Interface
import VisualizationData.Thunk

import Control.Monad ((>=>))

data LazyListThunk a =
  AppendThenReverseThunk (LazyListRef a) [a]

data LazyList a
  = Nil
  | Cons a (LazyListRef a)

type LazyListRef a = Thunk (LazyListThunk a) (LazyList a)

lazyListFromList :: [a] -> LazyListRef a
lazyListFromList =
  \case
    [] ->
      wrapThunk Nil
    x:xs ->
      wrapThunk (Cons x (lazyListFromList xs))

toWHNF :: LazyListThunk a -> LazyList a
toWHNF (AppendThenReverseThunk xs ys) =
  case forceWHNF xs of
    Nil ->
      forceWHNF (lazyListFromList (reverse ys))
    Cons x xs' ->
      Cons x $ createThunk $ AppendThenReverseThunk xs' ys

forceWHNF :: LazyListRef a -> LazyList a
forceWHNF =
  forceThunk toWHNF

evalStep :: LazyListRef a -> Maybe (LazyListRef a)
evalStep ref =
  case forceWHNF ref of
    Nil -> Nothing
    Cons _ ref' -> Just ref'

twoEvalStep :: LazyListRef a -> Maybe (LazyListRef a)
twoEvalStep =
  evalStep >=> evalStep

forceLazyList :: LazyListRef a -> [a]
forceLazyList xs =
  case forceWHNF xs of
    Nil -> []
    Cons x xs' -> x : forceLazyList xs'

data AQueue a =
  AQueue
  { frontList :: LazyListRef a
  , frontLen :: Int
  , rearList :: [a]
  , rearLen :: Int
  }

instance Queue AQueue where
  qempty = empty
  qsnoc = snoc
  quncons = uncons
  -- it is important to implement this manually, because the default implementation via quncons has the side-effect of evaluating thunks
  qnull q =
    frontLen q == 0

empty :: AQueue a
empty =
  AQueue
    { frontList = wrapThunk Nil
    , frontLen = 0
    , rearList = []
    , rearLen = 0
    }

null :: AQueue a -> Bool
null aq =
  frontLen aq == 0

toList :: AQueue a -> [a]
toList aq =
  forceLazyList (frontList aq) ++ reverse (rearList aq)

mkAQueue :: LazyListRef a -> Int -> [a] -> Int -> AQueue a
mkAQueue front frontL rear rearL =
  if frontL > rearL then
    AQueue
      { frontList = front
      , frontLen = frontL
      , rearList = rear
      , rearLen = rearL
      }
  else
    AQueue
      { frontList = createThunk (AppendThenReverseThunk front rear)
      , frontLen = frontL + rearL
      , rearList = []
      , rearLen = 0
      }

fromList :: [a] -> AQueue a
fromList xs =
  AQueue
    { frontList = lazyListFromList xs
    , frontLen = length xs
    , rearList = []
    , rearLen = 0
    }

uncons :: AQueue a -> Maybe (a, AQueue a)
uncons aq =
  case forceWHNF (frontList aq) of
    Nil ->
      Nothing
    Cons x front' ->
      Just (x, mkAQueue front' (frontLen aq - 1) (rearList aq) (rearLen aq))

head :: AQueue a -> Maybe a
head q =
  fst <$> uncons q

tail :: AQueue a -> Maybe (AQueue a)
tail q =
  snd <$> uncons q

snoc :: AQueue a -> a -> AQueue a
snoc aq x =
  if frontLen aq == 0 then
    AQueue
      { frontList = lazyListFromList [x]
      , frontLen = 1
      , rearList = []
      , rearLen = 0
      }
  else
    mkAQueue (frontList aq) (frontLen aq) (x : rearList aq) (rearLen aq + 1)
