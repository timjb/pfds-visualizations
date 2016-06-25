{-# LANGUAGE LambdaCase #-}

module VisualizationData.LenList where

import Prelude hiding (reverse)
import qualified Data.List as L

data LenList a = LenList { length :: !Int, items :: [a] }

instance Show a => Show (LenList a) where
  show ll = "fromNormalList " ++ show (items ll)

instance Eq a => Eq (LenList a) where
  LenList n xs == LenList m ys =
    n == m && xs == ys

fromNormalList :: [a] -> LenList a
fromNormalList xs =
  LenList (L.length xs) xs

instance Monoid (LenList a) where
  mempty = LenList 0 []
  mappend (LenList n xs) (LenList m ys) =
    LenList (n+m) (xs++ys)

singleton :: a -> LenList a
singleton x =
  LenList 1 [x]

uncons :: LenList a -> Maybe (a, LenList a)
uncons =
  \case
    LenList _ [] -> Nothing
    LenList n (x:xs) -> Just (x, LenList (n-1) xs)

cons :: a -> LenList a -> LenList a
cons x (LenList n xs) =
  LenList (succ n) (x : xs)

reverse :: LenList a -> LenList a
reverse (LenList n xs) =
  LenList n (L.reverse xs)

null :: LenList a -> Bool
null = \case
  LenList 0 _ -> True
  LenList _ _ -> False