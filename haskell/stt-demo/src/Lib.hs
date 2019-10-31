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
                                        runState, runStateT, MonadState, execStateT)
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
import System.Random.Xorshift128Plus   (Gen(..), initialize, next)
import Control.Lens.Lens ((&))
import Control.Lens ((.~), (^.), (.=), (%=), use)
import Data.StateRef (newRef)
import ImpureContainers.PrimRef (newAlignedPinned, newPinned)
import Foreign.C (CInt)
import Control.Arrow (first)

import qualified Data.HashMap.Mutable.Basic as HM
import qualified Data.Heap.Mutable.ModelD   as HPM
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           Debug.Trace

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
  let appState0 = AppState (initialize 123) "test"
  appState1 <- flip execStateT appState0 $ primTransExample (Point 1 1) (Point 0 0)
  putStrLn $ "appState = " <> show appState1

primTransExample :: MonadState AppState m => Point -> Point -> m ()
primTransExample src dst = do
  app <- get
  let ctx = Context app 0
  let ret = runST (primProc ctx)
  msg .= T.pack (show ret)
  where
    primProc :: PrimMonad m => Context -> m (T.Text, Context)
    primProc ctx = flip runStateT ctx $ do --flip runStateT ctx $ do
      alg <- primBuildAlg dst
      path <- primAlgorithm alg src
      return $ T.pack $ show path

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

-- primAlgorithm :: forall m p . (PrimMonad m, Show p) => Alg (StateT Context m) p -> p -> StateT Context m [p]
primBuildAlg :: forall m . PrimMonad m => Point -> StateT Context m (Alg (StateT Context m) Point)
primBuildAlg dst = do
    -- flip runStateT ctx $ do
      -- undefined :: StateT Context m (Alg m Point)
      let select1 = select
      let step1 = step
      let stopCond1 = stopCond
      return $ Alg { select = select1, step = step1, stopCond = stopCond1 }
  where
    select :: PrimMonad m => StateT Context m Direction
    select = do
      g0 <- use (appState . gen)
      let (r, g1) = next g0
      appState . gen .= g1
      return $ toEnum $ fromIntegral r `mod` 4

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
