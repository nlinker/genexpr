{-# LANGUAGE QuasiQuotes #-}

module Memo where

import Control.Concurrent.MVar    (MVar, modifyMVar_, newMVar, readMVar)
import Debug.Trace                (traceM)
import System.IO.Unsafe           (unsafePerformIO)
import Text.InterpolatedString.QM (qm)

import qualified Data.Map as M

curry2 :: ((a, b) -> c) -> a -> b -> c
curry2 f x y = f (x, y)

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f (x, y) = f x y

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

curry4 :: ((a, b, c, d) -> e) -> a -> b -> c -> d -> e
curry4 f w x y z = f (w, x, y, z)

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (w, x, y, z) = f w x y z

makeMemo :: MVar (M.Map a b)
makeMemo = unsafePerformIO $ newMVar M.empty

{-# NOINLINE makeMemo #-}
memo :: (Ord a, Show a) => MVar (M.Map a b) -> (a -> b) -> a -> b
memo cache f x =
  unsafePerformIO $ do
    m <- readMVar cache
    case M.lookup x m of
      Nothing -> do
        let r = f x
        modifyMVar_ cache (return . M.insert x r)
        traceM [qm| k={x}  size={M.size m}|]
        return r
      Just r -> return r

memo2 :: (Ord a, Ord b, Show a, Show b) => MVar (M.Map (a, b) c) -> (a -> b -> c) -> a -> b -> c
memo2 cache = curry2 . memo cache . uncurry2

memo3 ::
     (Ord a, Ord b, Ord c, Show a, Show b, Show c) => MVar (M.Map (a, b, c) d) -> (a -> b -> c -> d) -> a -> b -> c -> d
memo3 cache = curry3 . memo cache . uncurry3

memo4 ::
     (Ord a, Ord b, Ord c, Ord d, Show a, Show b, Show c, Show d)
  => MVar (M.Map (a, b, c, d) e)
  -> (a -> b -> c -> d -> e)
  -> a
  -> b
  -> c
  -> d
  -> e
memo4 cache = curry4 . memo cache . uncurry4
