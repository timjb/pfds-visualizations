{-# LANGUAGE LambdaCase, DeriveAnyClass, ViewPatterns #-}

module VisualizationData.Queue.RealTime where

import VisualizationData.Queue.Interface
import VisualizationData.Thunk

import Control.Arrow (second)
import Data.Either (either)

data LazyListThunk a
  = AppendReverseThunk !(LazyListRef a) !(LazyList a) ![a]

data LazyList a
  = Nil
  | Cons !a !(LazyListRef a)

type LazyListRef a = Thunk (LazyListThunk a) (LazyList a)

lazyListFromList :: [a] -> LazyListRef a
lazyListFromList [] = wrapThunk Nil
lazyListFromList (x:xs) =
  wrapThunk (Cons x (lazyListFromList xs))

toWHNF :: LazyListThunk a -> LazyList a
toWHNF (AppendReverseThunk xs rs ys) =
  case (forceWHNF xs, ys) of
    (Nil, []) -> rs
    (Cons x xs', y:ys') ->
      Cons x $ createThunk $ AppendReverseThunk xs' (Cons y (wrapThunk rs)) ys'

forceWHNF :: LazyListRef a -> LazyList a
forceWHNF = forceThunk toWHNF

evalStep :: LazyListRef a -> Maybe (LazyListRef a)
evalStep ref =
  case forceWHNF ref of
    Nil -> Nothing
    Cons _ ref' -> Just ref'

forceLazyList :: LazyListRef a -> [a]
forceLazyList xs =
  case forceWHNF xs of
    Nil -> []
    Cons x xs' -> x : forceLazyList xs'

-- this is the same as forceLazyList but without the side-effect of evaluating the thunks
lazyListToList :: LazyListRef a -> [a]
lazyListToList ref =
  case either toWHNF id (readThunk ref) of
    Nil -> []
    Cons x xs -> x : lazyListToList xs

data RTQueue a =
  RTQueue
  { frontList :: !(LazyListRef a)
  , frontLen :: !Int
  , rearList :: ![a]
  , rearLen :: !Int
  , schedule :: !(LazyListRef a)
  , scheduleStepNeeded :: !Bool
  }

scheduleIndex :: RTQueue a -> Int
scheduleIndex queue =
  rearLen queue - (if scheduleStepNeeded queue then 1 else 0)

instance Show a => Show (RTQueue a) where
  show queue = "fromList (" ++ show (toList queue) ++ ")"

instance Eq a => Eq (RTQueue a) where
  xs == ys = toList xs == toList ys

instance Queue RTQueue where
  qempty = empty
  qsnoc = snoc
  quncons = uncons
  -- it is important to implement this manually, because the default implementation via quncons has the side-effect of evaluating thunks
  qnull q = frontLen q == 0

empty :: RTQueue a
empty = let front = wrapThunk Nil in RTQueue front 0 [] 0 front False

null :: RTQueue a -> Bool
null (RTQueue _ k _ _ _ _) = k == 0

toList :: RTQueue a -> [a]
toList (RTQueue front _ rear _ _ _) = lazyListToList front ++ reverse rear

schedStep :: RTQueue a -> RTQueue a
schedStep rtq@(RTQueue front frontL rear rearL sched schedStepNeeded)
  | schedStepNeeded = 
    case forceWHNF sched of
      Nil -> rtq
      Cons _ sched' -> RTQueue front frontL rear rearL sched' False
  | otherwise = rtq

-- without scheduled step!
mkRTQueue :: LazyListRef a -> Int -> [a] -> Int -> LazyListRef a -> RTQueue a
mkRTQueue front frontL rear rearL sched
  | frontL > rearL = RTQueue front frontL rear rearL sched True
  | otherwise =
    let front' = createThunk (AppendReverseThunk front Nil rear)
    in RTQueue front' (frontL + rearL) [] 0 front' False

fromList :: [a] -> RTQueue a
fromList xs =
  let front = lazyListFromList xs
  in RTQueue front (length xs) [] 0 front False

uncons' :: RTQueue a -> Maybe (a, RTQueue a)
uncons' (schedStep -> RTQueue front frontL rear rearL sched _) =
  case forceWHNF front of
    Nil -> Nothing
    Cons x front' -> Just (x, mkRTQueue front' (frontL-1) rear rearL sched)

uncons :: RTQueue a -> Maybe (a, RTQueue a)
uncons queue = second schedStep <$> uncons' queue

head :: RTQueue a -> Maybe a
head q = fst <$> uncons q

tail' :: RTQueue a -> Maybe (RTQueue a)
tail' = fmap snd . uncons'

tail :: RTQueue a -> Maybe (RTQueue a)
tail q = snd <$> uncons q

snoc' :: RTQueue a -> a -> RTQueue a
snoc' (schedStep -> RTQueue front frontL rear rearL sched _) x
  | frontL == 0 = let front' = lazyListFromList [x]
                  in RTQueue front' 1 [] 0 front' False
  | otherwise   = mkRTQueue front frontL (x:rear) (rearL+1) sched

snoc :: RTQueue a -> a -> RTQueue a
snoc queue x = schedStep (snoc' queue x)
