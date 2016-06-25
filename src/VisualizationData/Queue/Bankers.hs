module VisualizationData.Queue.Bankers where

import VisualizationData.Queue.Interface
import qualified Data.List as L
import qualified VisualizationData.LenList as LL

data BQueue a
  = BQueue
  { heads :: LL.LenList a
  , tails :: LL.LenList a
  }

instance Show a => Show (BQueue a) where
  show q =
    "fromList " ++ show (toList q)

instance Eq a => Eq (BQueue a) where
  q == r =
    toList q == toList r

instance Queue BQueue where
  qempty = empty
  qsnoc = snoc
  quncons = uncons

toList :: BQueue a -> [a]
toList bq =
  LL.items (heads bq) ++ L.reverse (LL.items (tails bq))

fromList :: [a] -> BQueue a
fromList xs =
  BQueue { heads = LL.fromNormalList xs, tails = mempty }

empty :: BQueue a
empty =
  BQueue { heads = mempty, tails = mempty }

null :: BQueue a -> Bool
null bq =
  LL.null (heads bq)

mappendReverse' :: [a] -> [a] -> [a]
mappendReverse' =
  go []
  where
    go rs [] [] = rs
    go rs (x:xs) (y:ys) = x : go (y:rs) xs ys
    go _ _ _ = error "not expected!"

mappendReverse :: LL.LenList a -> LL.LenList a -> LL.LenList a
mappendReverse (LL.LenList n xs) (LL.LenList m ys) =
  LL.LenList (n+m) (mappendReverse' xs ys)

mkBQueue :: LL.LenList a -> LL.LenList a -> BQueue a
mkBQueue xs ys =
  if LL.length xs > LL.length ys then
    BQueue { heads = xs, tails = ys }
  else
    BQueue { heads = mappendReverse xs ys, tails = mempty }

uncons :: BQueue a -> Maybe (a, BQueue a)
uncons bq =
  case LL.uncons (heads bq) of
    Nothing -> Nothing
    Just (x, xs) -> Just (x, mkBQueue xs (tails bq))

tail :: BQueue a -> Maybe (BQueue a)
tail q =
  snd <$> uncons q

snoc :: BQueue a -> a -> BQueue a
snoc bq y =
  if LL.null (heads bq) then
    BQueue { heads = LL.singleton y, tails = mempty }
  else
    mkBQueue (heads bq) (LL.cons y (tails bq))
