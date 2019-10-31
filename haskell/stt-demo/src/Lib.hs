{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}

module Lib where

import Control.Lens.TH                 (makeLenses)
import Control.Concurrent.MVar         (MVar, modifyMVar_, newMVar, readMVar)
import Control.Monad                   (filterM, forM_, when)
import Control.Monad.Identity          (Identity, runIdentity)
import Control.Monad.IO.Class          (liftIO, MonadIO)
import Control.Monad.Primitive         (PrimMonad, PrimState, primitive)
import Control.Monad.ST                (ST, runST)
import Control.Monad.ST.Trans          (runSTT)
import Control.Monad.ST.Trans.Internal (STT(..), STTRet(..), liftST)
import Control.Monad.State.Strict      (MonadState, State, evalStateT, get, lift, modify, put,
                                        runState, runStateT, MonadState)
import Control.Monad.State.Strict      (StateT)
import Data.Char                       (ord)
import Data.Coerce                     (coerce)
import Data.Hashable                   (Hashable)
import Data.Maybe                      (fromMaybe, isJust)
import Data.STRef                      (modifySTRef)
import Data.STRef.Strict               (STRef, modifySTRef', newSTRef, readSTRef)
import GHC.Generics                    (Generic)
import System.IO.Unsafe                (unsafePerformIO)
import Text.InterpolatedString.QM      (qm)

import qualified Data.HashMap.Mutable.Basic as HM
import qualified Data.Heap.Mutable.ModelD   as HPM
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           Debug.Trace
import Control.Lens.Lens ((&))
import Control.Lens ((.~), (^.), (.=), (%=))
import Data.StateRef (newRef)
import ImpureContainers.PrimRef (newAlignedPinned, newPinned)
import Foreign.C (CInt)
import Control.Arrow (first)

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

data AppState =
  AppState
    { _field :: [Point]
    , _index :: Int
    , _msg  :: T.Text
    }
    deriving (Eq, Show)

data Context =
  Context
    { _appState :: AppState
    , _ctxState :: Integer
    }
    deriving (Eq, Show)

makeLenses ''AppState
makeLenses ''Context

instance (Monad m) => PrimMonad (STT s m) where
  type PrimState (STT s m) = s
  primitive f =
    STT $ \s ->
      case f s of
        (# t, a #) -> return (STTRet t a)
  {-# INLINE primitive #-}

-- State monad on top of ST monad
transExample :: MonadState AppState m => String -> m (Integer, Context)
transExample str = do
  app <- get
  let ctx = Context app 0
  runStateT proc ctx
  where
    proc :: MonadState Context m => m Integer
    proc = do
      ctx <- get
      x <- return $ runST $ do
        ref <- newSTRef 0
        forM_ str $ \c -> do
          -- undefined :: ST s ()
          modifySTRef' ref (+1)
        readSTRef ref
      ctxState .= x
      return x

primTransExample :: MonadState AppState m => String -> m ()
primTransExample str = do
  app <- get
  let ctx = Context app 0
  let ret = runST (primProc ctx)
  msg .= T.pack (show ret)
  where
    primProc :: forall m . PrimMonad m => Context -> m (T.Text, Context)
    primProc ctx = do --flip runStateT ctx $ do
      alg <- primBuildAlg ctx (Point 0 0)
      pair <- flip runStateT ctx $ primAlgorithm alg (Point 1 1)
      return $ first (T.pack . show) pair   

-- (MonadState Context m)
primBuildAlg :: forall m . PrimMonad m => Context -> Point -> m (Alg (StateT Context m) Point)
primBuildAlg ctx dst = do
    -- flip runStateT ctx $ do
      -- undefined :: StateT Context m (Alg m Point) 
      let select1 = select
      let step1 = step
      let stopCond1 = stopCond
      return $ Alg { select = select1, step = step1, stopCond = stopCond1 }
  where
    select :: PrimMonad m => StateT Context m Direction
    select = do
      ref <- lift $ newPinned (0 :: CInt)
      modify $ \ctx -> let n = ctx ^. ctxState in ctx & ctxState .~ (if even n then n `div` 2 else n * 3 + 1)
      ctx <- get
      return $ toEnum $ fromIntegral (ctx ^. ctxState) `mod` 4

    step :: PrimMonad m => Direction -> Point -> StateT Context m Point
    step d p = return $ go d p
      where
        go d (Point i j) =
          case d of
            U -> Point (i - 1) (j)
            D -> Point (i + 1) (j)
            L -> Point (i) (j - 1)
            R -> Point (i) (j + 1)

    stopCond :: PrimMonad m => Point -> StateT Context m Bool
    stopCond p = return $ dst == p

-- MonadState Context m
primAlgorithm :: forall m p . (PrimMonad m, Show p) => Alg (StateT Context m) p -> p -> StateT Context m [p]
primAlgorithm alg start = go start []
  where
    go :: p -> [p] -> StateT Context m [p]
    go p1 xs = do
      stop <- stopCond alg p1
      if stop then return xs
      else do
        dir <- select alg
        p2 <- step alg dir p1
        traceM $ show p2
        go p2 (p2:xs)

-- StateT Context m a
algorithm :: forall m p . (MonadState Context m, Show p) => Alg m p -> p -> m [p]
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
testAlg = undefined -- do
--  let appState = AppState [] 0 "test"
--  let ctx = Context appState 13
--  path <- flip evalStateT ctx $ do
--        alg <- buildAlg (Point 0 0)
--        algorithm alg (Point 2 2)
--  putStrLn $ "path = " <> show path


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
makeMemo = unsafePerformIO $ do
  v <- newMVar M.empty
  return v
{-# NOINLINE makeMemo #-}

memo :: (Ord a, Show a) => MVar (M.Map a b) -> (a -> b) -> a -> b
memo cache f x = unsafePerformIO $ do
    m <- readMVar cache
    case M.lookup x m of
      Nothing -> do
        let  r = f x
        modifyMVar_ cache (return . M.insert x r)
        traceM [qm| k={x}  size={M.size m}|]
        return r
      Just r  -> return r

memo2 :: (Ord a, Ord b, Show a, Show b) => MVar (M.Map (a, b) c) -> (a -> b -> c) -> a -> b -> c
memo2 cache = curry2 . memo cache . uncurry2

memo3 :: (Ord a, Ord b, Ord c, Show a, Show b, Show c) =>
     MVar (M.Map (a, b, c) d) -> (a -> b -> c -> d) -> a -> b -> c -> d
memo3 cache = curry3 . memo cache . uncurry3

memo4
  :: (Ord a, Ord b, Ord c, Ord d, Show a, Show b, Show c, Show d) =>
     MVar (M.Map (a, b, c, d) e)
     -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> e
memo4 cache = curry4 . memo cache . uncurry4
