{-# LANGUAGE LambdaCase #-}

module RealTimeQueue where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad ((>=>))

data LazyListThunk a =
  AppendReverseThunk (LazyListRef a) (LazyList a) [a]

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
toWHNF (AppendReverseThunk xs rs ys) =
  case (forceWHNF xs, ys) of
    (Nil, []) -> rs
    (Cons x xs', y:ys') ->
      Cons x $ createIORef $ Left $
      AppendReverseThunk xs' (Cons y (toRef rs)) ys'

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

data RTQueue a =
  RTQueue
  { frontList :: LazyListRef a
  , frontLen :: Int
  , rearList :: [a]
  , rearLen :: Int
  }

empty :: RTQueue a
empty = RTQueue (toRef Nil) 0 [] 0

null :: RTQueue a -> Bool
null (RTQueue _ k _ _) = k == 0

toList :: RTQueue a -> [a]
toList (RTQueue front _ rear _) = forceLazyList front ++ reverse rear

mkRTQueue :: LazyListRef a -> Int -> [a] -> Int -> RTQueue a
mkRTQueue front frontL rear rearL
  | frontL > rearL = RTQueue front frontL rear rearL
  | otherwise =
    RTQueue (createIORef (Left (AppendReverseThunk front Nil rear))) (frontL + rearL) [] 0

fromList :: [a] -> RTQueue a
fromList xs = RTQueue (lazyListFromList xs) (length xs) [] 0

uncons :: RTQueue a -> Maybe (a, RTQueue a)
uncons (RTQueue front frontL rear rearL) =
  case forceWHNF front of
    Nil -> Nothing
    Cons x front' -> Just (x, mkRTQueue front' (frontL-1) rear rearL)

head :: RTQueue a -> Maybe a
head q = fst <$> uncons q

tail :: RTQueue a -> Maybe (RTQueue a)
tail q = snd <$> uncons q

snoc :: RTQueue a -> a -> RTQueue a
snoc (RTQueue front frontL rear rearL) x
  | frontL == 0 = RTQueue (lazyListFromList [x]) 1 [] 0
  | otherwise   = mkRTQueue front frontL (x:rear) (rearL+1)
