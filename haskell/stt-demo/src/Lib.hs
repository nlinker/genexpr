{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE Strict #-}

module Lib where

import Data.Hashable                   (Hashable)
import Control.Monad.ST                (ST, runST)
import Control.Monad.ST.Trans          (STT, runSTT)
import Control.Monad.ST.Trans.Internal (liftST)
import Data.Maybe                      (isJust, fromMaybe)
import GHC.Generics                    (Generic)
import Control.Monad                   (forM_, when, filterM)
import Control.Monad.Primitive         (PrimMonad, PrimState)

import qualified Data.Heap.Mutable.ModelD   as HPM
import qualified Data.HashMap.Mutable.Basic as HM
import Debug.Trace

newtype Min =
  Min Int
  deriving (Show, Read, Eq, Ord)

data Point = Point Int Int deriving (Eq, Show, Generic, Hashable)
instance Ord Point where
  compare (Point i1 j1) (Point i2 j2) = compare (i1, i2) (j1, j2)

someAlgorithm :: forall m p . (Monad m, Hashable p, Ord p, Show p) => p -> p -> (p -> m Int) -> m [p]
someAlgorithm src dst rang = do
  path <- runSTT $ do
    table <- HM.new
    HM.insert table src dst
    HM.insert table dst src
    has <- member table src
    traceShowM $ "has = " <> show has
    return [] 
  traceShowM path
  return path
  where
    -- member :: (Monad m, Hashable k, Eq k) => HM.MHashMap s k a -> k -> STT s m Bool
    member table p = do
      value <- HM.lookup table p
      return $ isJust value 
