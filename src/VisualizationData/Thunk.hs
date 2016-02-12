{-# LANGUAGE LambdaCase #-}

module VisualizationData.Thunk
  ( Thunk
  , mkThunk
  , createThunk
  , wrapThunk
  , readThunk
  , forceThunk
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

newtype Thunk t a = Thunk { getThunk :: IORef (Either t a) }

mkThunk :: Either t a -> Thunk t a
mkThunk = unsafePerformIO . fmap Thunk . newIORef

createThunk :: t -> Thunk t a
createThunk = mkThunk . Left

wrapThunk :: a -> Thunk t a
wrapThunk = mkThunk . Right

readThunk :: Thunk t a -> Either t a
readThunk = unsafePerformIO . readIORef . getThunk

forceThunk :: (t -> a) -> Thunk t a -> a
forceThunk step (Thunk ref) =
  unsafePerformIO $ do
    readIORef ref >>= \case
      Left t -> do
        let v = step t
        writeIORef ref (Right v)
        return v
      Right v -> return v
