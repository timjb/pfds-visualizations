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
lazyListFromList [] = wrapThunk Nil
lazyListFromList (x:xs) =
  wrapThunk (Cons x (lazyListFromList xs))

toWHNF :: LazyListThunk a -> LazyList a
toWHNF (AppendThenReverseThunk xs ys) =
  case forceWHNF xs of
    Nil -> forceWHNF (lazyListFromList (reverse ys))
    Cons x xs' ->
      Cons x $ createThunk $ AppendThenReverseThunk xs' ys

forceWHNF :: LazyListRef a -> LazyList a
forceWHNF = forceThunk toWHNF

evalStep :: LazyListRef a -> Maybe (LazyListRef a)
evalStep ref =
  case forceWHNF ref of
    Nil -> Nothing
    Cons _ ref' -> Just ref'

twoEvalStep :: LazyListRef a -> Maybe (LazyListRef a)
twoEvalStep = evalStep >=> evalStep

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
  qnull q = frontLen q == 0

empty :: AQueue a
empty = AQueue (wrapThunk Nil) 0 [] 0

null :: AQueue a -> Bool
null (AQueue _ k _ _) = k == 0

toList :: AQueue a -> [a]
toList (AQueue front _ rear _) = forceLazyList front ++ reverse rear

mkAQueue :: LazyListRef a -> Int -> [a] -> Int -> AQueue a
mkAQueue front frontL rear rearL
  | frontL > rearL = AQueue front frontL rear rearL
  | otherwise =
    AQueue (createThunk (AppendThenReverseThunk front rear)) (frontL + rearL) [] 0

fromList :: [a] -> AQueue a
fromList xs = AQueue (lazyListFromList xs) (length xs) [] 0

uncons :: AQueue a -> Maybe (a, AQueue a)
uncons (AQueue front frontL rear rearL) =
  case forceWHNF front of
    Nil -> Nothing
    Cons x front' -> Just (x, mkAQueue front' (frontL-1) rear rearL)

head :: AQueue a -> Maybe a
head q = fst <$> uncons q

tail :: AQueue a -> Maybe (AQueue a)
tail q = snd <$> uncons q

snoc :: AQueue a -> a -> AQueue a
snoc (AQueue front frontL rear rearL) x
  | frontL == 0 = AQueue (lazyListFromList [x]) 1 [] 0
  | otherwise   = mkAQueue front frontL (x:rear) (rearL+1)