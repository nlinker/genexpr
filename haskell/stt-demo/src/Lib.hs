{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}

module Lib where

import Control.Arrow                   (first)
import Control.Concurrent.MVar         (MVar, modifyMVar_, newMVar, readMVar)
import Control.Lens                    (use, (%=), (.=), (.~), (^.))
import Control.Lens.Lens               ((&))
import Control.Lens.TH                 (makeLenses)
import Control.Monad                   (filterM, forM_, when)
import Control.Monad.Identity          (Identity, runIdentity)
import Control.Monad.IO.Class          (MonadIO, liftIO)
import Control.Monad.Primitive         (PrimMonad, PrimState, primitive)
import Control.Monad.ST                (ST, runST)
import Control.Monad.ST.Trans          (runSTT)
import Control.Monad.ST.Trans.Internal (STT(..), STTRet(..), liftST)
import Control.Monad.State.Strict      (MonadState, MonadState, State, evalStateT, execStateT, get,
                                        lift, modify, put, runState, runStateT)
import Control.Monad.State.Strict      (StateT)
import Data.Char                       (ord)
import Data.Coerce                     (coerce)
import Data.Hashable                   (Hashable)
import Data.Maybe                      (fromMaybe, isJust)
import Data.StateRef                   (newRef)
import Data.STRef                      (modifySTRef)
import Data.STRef.Strict               (STRef, modifySTRef', newSTRef, readSTRef)
import Foreign.C                       (CInt)
import GHC.Generics                    (Generic)
import ImpureContainers.PrimRef        (newAlignedPinned, newPinned)
import System.IO.Unsafe                (unsafePerformIO)
import System.Random.Xorshift128Plus   (Gen(..), initialize, next)
import Text.InterpolatedString.QM      (qm)

import qualified Data.HashMap.Mutable.Basic as HM
import qualified Data.Heap.Mutable.ModelD   as HPM
import qualified Data.Map                   as M
import qualified Data.Text                  as T

import Debug.Trace

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
    { _gen :: Gen
    , _msg :: T.Text
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

testAlg :: IO ()
testAlg = do
  let rngState = initialize 3
  let appState0 = AppState rngState "test"
  appState1 <- flip execStateT appState0 $ primTransExample (Point 1 1) (Point 0 0)
  putStrLn $ "message = " <> show (appState1 ^. msg)

primTransExample :: MonadState AppState m => Point -> Point -> m ()
primTransExample src dst = do
  app1 <- get
  traceM [qm| cxt before = {app1} |]
  let app2 = runST (primProc app1)
  traceM [qm| cxt after = {app2} |]
  put $ app2
  where
    primProc :: PrimMonad m => AppState -> m AppState
    primProc ctx = flip execStateT ctx $ do
      alg <- primBuildAlg dst
      path <- primAlgorithm alg src
      msg .= T.pack ("steps count = " <> show (length path) <> ", path = " <> show path)

-- MonadState Context m
primAlgorithm :: forall m p . (PrimMonad m, Show p) => Alg (StateT AppState m) p -> p -> StateT AppState m [p]
primAlgorithm alg start = go start []
  where
    go :: p -> [p] -> StateT AppState m [p]
    go p1 xs = do
      stop <- stopCond alg p1
      if stop then return xs
      else do
        dir <- select alg
        p2 <- step alg dir p1
        go p2 (p2:xs)

primBuildAlg :: forall m . PrimMonad m => Point -> StateT AppState m (Alg (StateT AppState m) Point)
primBuildAlg dst = do
    -- flip runStateT ctx $ do
      let select1 = select
      let step1 = step
      let stopCond1 = stopCond
      return $ Alg { select = select1, step = step1, stopCond = stopCond1 }
  where
    select :: PrimMonad m => StateT AppState m Direction
    select = do
      g0 <- use gen
      let (r, g1) = next g0
      gen .= g1
      return $ toEnum $ fromIntegral r `mod` 4

    step :: PrimMonad m => Direction -> Point -> StateT AppState m Point
    step d p = return $ go d p
      where
        go d (Point i j) =
          case d of
            U -> Point (i - 1) (j)
            D -> Point (i + 1) (j)
            L -> Point (i) (j - 1)
            R -> Point (i) (j + 1)

    stopCond :: PrimMonad m => Point -> StateT AppState m Bool
    stopCond p = return $ dst == p

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

-- ST monad on top of State monad
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

-- for runSTT working
--instance (Monad m) => PrimMonad (STT s m) where
--  type PrimState (STT s m) = s
--  primitive f =
--    STT $ \s ->
--      case f s of
--        (# t, a #) -> return (STTRet t a)
--  {-# INLINE primitive #-}
