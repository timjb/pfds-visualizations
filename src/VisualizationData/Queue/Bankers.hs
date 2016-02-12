module VisualizationData.Queue.Bankers where

import qualified Data.List as L
import qualified VisualizationData.LenList as LL
import VisualizationData.Queue.Interface

data BQueue a =
  BQueue { heads :: LL.LenList a
         , tails :: LL.LenList a
         }

instance Show a => Show (BQueue a) where
  show q = "fromList " ++ show (toList q)

instance Eq a => Eq (BQueue a) where
  q == r = toList q == toList r

instance Queue BQueue where
  qempty = empty
  qsnoc = snoc
  quncons = uncons

toList :: BQueue a -> [a]
toList (BQueue xs ys) = LL.items xs ++ L.reverse (LL.items ys)

fromList :: [a] -> BQueue a
fromList xs = BQueue (LL.fromNormalList xs) mempty

empty :: BQueue a
empty = BQueue mempty mempty

null :: BQueue a -> Bool
null (BQueue xs _) = LL.null xs

mappendReverse' :: [a] -> [a] -> [a]
mappendReverse' = go []
  where
    go rs [] [] = rs
    go rs (x:xs) (y:ys) = x : go (y:rs) xs ys

mappendReverse :: LL.LenList a -> LL.LenList a -> LL.LenList a
mappendReverse (LL.LenList n xs) (LL.LenList m ys) =
  LL.LenList (n+m) (mappendReverse' xs ys)

mkBQueue :: LL.LenList a -> LL.LenList a -> BQueue a
mkBQueue xs ys =
  if LL.length xs > LL.length ys
    then BQueue xs ys
    else BQueue (mappendReverse xs ys) mempty

uncons :: BQueue a -> Maybe (a, BQueue a)
uncons (BQueue xss ys) =
  case LL.uncons xss of
    Nothing -> Nothing
    Just (x, xs) -> Just (x, mkBQueue xs ys)

tail :: BQueue a -> Maybe (BQueue a)
tail q = snd <$> uncons q

snoc :: BQueue a -> a -> BQueue a
snoc (BQueue xs ys) y
  | LL.null xs = BQueue (LL.singleton y) mempty
  | otherwise  = mkBQueue xs (LL.cons y ys)
