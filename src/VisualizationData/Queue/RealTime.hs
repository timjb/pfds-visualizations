{-# LANGUAGE LambdaCase #-}

module VisualizationData.Queue.RealTime where
  
import VisualizationData.Queue.Interface

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad ((>=>))

data LazyListThunk a =
  AppendReverseThunk !(LazyListRef a) !(LazyList a) ![a]

data LazyList a
  = Nil
  | Cons !a !(LazyListRef a)

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

forceLazyList :: LazyListRef a -> [a]
forceLazyList xs =
  case forceWHNF xs of
    Nil -> []
    Cons x xs' -> x : forceLazyList xs'

data RTQueue a =
  RTQueue
  { frontList :: !(LazyListRef a)
  , frontLen :: !Int
  , rearList :: ![a]
  , rearLen :: !Int
  , schedule :: !(LazyListRef a)
  }

instance Queue RTQueue where
  qempty = empty
  qsnoc = snoc
  quncons = uncons

empty :: RTQueue a
empty = let front = toRef Nil in RTQueue front 0 [] 0 front

null :: RTQueue a -> Bool
null (RTQueue _ k _ _ _) = k == 0

toList :: RTQueue a -> [a]
toList (RTQueue front _ rear _ _) = forceLazyList front ++ reverse rear

schedStep :: RTQueue a -> RTQueue a
schedStep rtq@(RTQueue front frontL rear rearL sched) =
  case forceWHNF sched of
    Nil -> rtq
    Cons _ sched' -> RTQueue front frontL rear rearL sched'

-- without scheduled step!
mkRTQueue :: LazyListRef a -> Int -> [a] -> Int -> LazyListRef a -> RTQueue a
mkRTQueue front frontL rear rearL sched
  | frontL > rearL = RTQueue front frontL rear rearL sched
  | otherwise =
    let newFront = createIORef (Left (AppendReverseThunk front Nil rear))
    in RTQueue newFront (frontL + rearL) [] 0 newFront

fromList :: [a] -> RTQueue a
fromList xs =
  let front = lazyListFromList xs
  in RTQueue front (length xs) [] 0 front

uncons :: RTQueue a -> Maybe (a, RTQueue a)
uncons (RTQueue front frontL rear rearL sched) =
  case forceWHNF front of
    Nil -> Nothing
    Cons x front' -> Just (x, mkRTQueue front' (frontL-1) rear rearL sched)

head :: RTQueue a -> Maybe a
head q = fst <$> uncons q

tail :: RTQueue a -> Maybe (RTQueue a)
tail q = snd <$> uncons q

snoc :: RTQueue a -> a -> RTQueue a
snoc (RTQueue front frontL rear rearL sched) x
  | frontL == 0 = let front = lazyListFromList [x] in RTQueue front 1 [] 0 front
  | otherwise   = mkRTQueue front frontL (x:rear) (rearL+1) sched
