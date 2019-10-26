{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}

module Lib where

import Control.Concurrent.MVar         (modifyMVar_, newMVar, readMVar)
import Control.Monad                   (filterM, forM_, when)
import Control.Monad.Identity          (runIdentity)
import Control.Monad.Primitive         (PrimMonad, PrimState, primitive)
import Control.Monad.ST                (ST, runST)
import Control.Monad.ST.Trans          (runSTT)
import Control.Monad.ST.Trans.Internal (STT(..), STTRet(..), liftST)
import Control.Monad.State             (MonadState, evalStateT, get, lift, modify, put)
import Data.Coerce                     (coerce)
import Data.Hashable                   (Hashable)
import Data.Maybe                      (fromMaybe, isJust)
import GHC.Generics                    (Generic)
import System.IO.Unsafe                (unsafePerformIO)
import Text.InterpolatedString.QM      (qm)

import qualified Data.HashMap.Mutable.Basic as HM
import qualified Data.Heap.Mutable.ModelD   as HPM
import qualified Data.Map                   as M
import           Debug.Trace

instance (Monad m) => PrimMonad (STT s m) where
  type PrimState (STT s m) = s
  primitive f =
    STT $ \s ->
      case f s of
        (# t, a #) -> return (STTRet t a)
  {-# INLINE primitive #-}

data Direction
  = U
  | D
  | L
  | R
  deriving (Eq, Ord, Enum, Show)

data Point =
  Point Int Int
  deriving (Eq, Generic, Hashable)

instance Ord Point where
  compare (Point i1 j1) (Point i2 j2) = compare (i1, i2) (j1, j2)

instance Show Point where
  show (Point i j) = "(" <> show i <> ", " <> show j <> ")"

data Alg m p where
  Alg :: (Monad m, Eq p) => {
    select :: m Direction,
    step :: Direction -> p -> m p,
    stopCond :: p -> m Bool
  } -> Alg m p

buildAlg :: forall m . MonadState Integer m => Point -> m (Alg m Point)
buildAlg dst = return $ Alg { select = select, step = memoStep, stopCond = stopCond }
  where
    select :: m Direction
    select = do
      modify $ \n -> if even n then n `div` 2 else n * 3 + 1
      state <- get
      return $ toEnum $ fromIntegral state `mod` 4

    memoStep :: Direction -> Point -> m Point
    memoStep d p = memo step (d, p)
    step (d, (Point i j)) = return $
      case d of
        U -> Point (i - 1) (j)
        D -> Point (i + 1) (j)
        L -> Point (i) (j - 1)
        R -> Point (i) (j + 1)

    stopCond :: Point -> m Bool
    stopCond p = return $ dst == p

algorithm :: forall m p . (Monad m, Show p) => Alg m p -> p -> m [p]
algorithm alg start = go start []
  where
    go :: p -> [p] -> m [p]
    go p1 xs = do
      stop <- stopCond alg p1
      if stop then return xs
      else do
        dir <- select alg
        p2 <- step alg dir p1
        traceM $ show p2
        go p2 (p2:xs)

testAlg :: IO ()
testAlg = do
  let state = 13
  path <- flip evalStateT state $ do
        alg <- buildAlg (Point 0 0)
        algorithm alg (Point 3 2)
  putStrLn $ "path = " <> show path

-----------------------------------
-- (taken from uglymemo-0.1.0.1) --

-- | Memoize the given function by allocating a memo table,
-- and then updating the memo table on each function call.
memoIO :: (Ord a, Show a)
       => (a -> b)           -- ^Function to memoize
       -> IO (a -> IO b)
memoIO f = do
    v <- newMVar M.empty
    let f' x = do
            m <- readMVar v
            case M.lookup x m of
                Nothing -> do 
                  let  r = f x
                  modifyMVar_ v (return . M.insert x r)
                  traceM [qm| k={x}  size={M.size m}|]
                  return r
                Just r  -> return r
    return f'

-- | The pure version of 'memoIO'.
memo :: (Ord a, Show a)
     => (a -> b)  -- ^Function to memoize
     -> (a -> b)
memo f = 
  let f' = unsafePerformIO (memoIO f) 
   in \x -> unsafePerformIO (f' x)
