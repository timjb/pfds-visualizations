{-# LANGUAGE LambdaCase #-}

module VisualizationData.Queue.Amortized where

import VisualizationData.Queue.Interface

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad ((>=>))

data LazyListThunk a =
  AppendThenReverseThunk (LazyListRef a) [a]

data LazyList a
  = Nil
  | Cons a (LazyListRef a)

type LazyListRef a = IORef (Either (LazyListThunk a) (LazyList a))

createIORef :: a -> IORef a
createIORef x = unsafePerformIO (newIORef x)

toRef :: LazyList a -> LazyListRef a
toRef = createIORef . Right

lazyListFromList :: [a] -> LazyListRef a
lazyListFromList [] = toRef Nil
lazyListFromList (x:xs) =
  toRef (Cons x (lazyListFromList xs))

toWHNF :: LazyListThunk a -> LazyList a
toWHNF (AppendThenReverseThunk xs ys) =
  case forceWHNF xs of
    Nil -> forceWHNF (lazyListFromList (reverse ys))
    Cons x xs' ->
      Cons x $ createIORef $ Left $
      AppendThenReverseThunk xs' ys

forceWHNF :: LazyListRef a -> LazyList a
forceWHNF ref =
  unsafePerformIO $ do
    readIORef ref >>= \case
      Left thunk -> do
        let whnf = toWHNF thunk
        writeIORef ref (Right whnf)
        return whnf
      Right lazyList -> return lazyList

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

empty :: AQueue a
empty = AQueue (toRef Nil) 0 [] 0

null :: AQueue a -> Bool
null (AQueue _ k _ _) = k == 0

toList :: AQueue a -> [a]
toList (AQueue front _ rear _) = forceLazyList front ++ reverse rear

mkAQueue :: LazyListRef a -> Int -> [a] -> Int -> AQueue a
mkAQueue front frontL rear rearL
  | frontL > rearL = AQueue front frontL rear rearL
  | otherwise =
    AQueue (createIORef (Left (AppendThenReverseThunk front rear))) (frontL + rearL) [] 0

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