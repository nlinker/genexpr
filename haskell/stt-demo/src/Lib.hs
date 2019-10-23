{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}

module Lib where

import Control.Monad                   (filterM, forM_, when)
import Control.Monad.Primitive         (PrimMonad, PrimState, primitive)
import Control.Monad.ST                (ST, runST)
import Control.Monad.ST.Trans          (runSTT)
import Control.Monad.ST.Trans.Internal (STT(..), STTRet(..), liftST)
import Data.Hashable                   (Hashable)
import Data.Maybe                      (fromMaybe, isJust)
import GHC.Generics                    (Generic)

import qualified Data.HashMap.Mutable.Basic as HM
import qualified Data.Heap.Mutable.ModelD   as HPM
import           Debug.Trace

newtype Min =
  Min Int
  deriving (Show, Read, Eq, Ord)

data Point =
  Point Int Int
  deriving (Eq, Show, Generic, Hashable)

instance Ord Point where
  compare (Point i1 j1) (Point i2 j2) = compare (i1, i2) (j1, j2)

instance (Monad m) => PrimMonad (STT s m) where
  type PrimState (STT s m) = s
  primitive f =
    STT $ \s ->
      case f s of
        (# t, a #) -> return (STTRet t a)
  {-# INLINE primitive #-}

someAlgorithm ::
     forall m p. (Monad m, Hashable p, Ord p, Show p)
  => p
  -> p
  -> (p -> m Int)
  -> m [p]
someAlgorithm src dst rang = do
  path <-
    runSTT $ do
      table <- HM.new
      HM.insert table src dst
      HM.insert table dst src
      has <- member table src
      traceShowM $ "has = " <> show has
      return [src, dst]
  traceShowM path
  return path
  where
    member table p = do
      value <- HM.lookup table p
      return $ isJust value
