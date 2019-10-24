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

import Control.Monad                   (filterM, forM_, when)
import Control.Monad.Primitive         (PrimMonad, PrimState, primitive)
import Control.Monad.ST                (ST, runST)
import Control.Monad.ST.Trans          (runSTT)
import Control.Monad.ST.Trans.Internal (STT(..), STTRet(..), liftST)
import Control.Monad.State             (lift)
import Data.Hashable                   (Hashable)
import Data.Maybe                      (fromMaybe, isJust)
import GHC.Generics                    (Generic)
import Text.InterpolatedString.QM      (qm)
import Control.Monad.Identity          (runIdentity)
import Data.Coerce                     (coerce)

import qualified Data.HashMap.Mutable.Basic as HM
import qualified Data.Heap.Mutable.ModelD   as HPM
import           Debug.Trace

data Direction
  = U
  | D
  | L
  | R
  deriving (Eq, Ord, Show, Generic, Hashable)

data Point =
  Point Int Int
  deriving (Eq, Show, Generic, Hashable)

instance Ord Point where
  compare (Point i1 j1) (Point i2 j2) = compare (i1, i2) (j1, j2)

newtype Min =
  Min Int
  deriving (Show, Read, Eq, Ord)

instance Semigroup Min where
  (<>) (Min a) (Min b) = Min (min a b)

instance Monoid Min where
  mempty = Min maxBound

newtype MyElement = MyElement { getMyElement :: Int }
  deriving (Show,Read,Eq,Ord)

data AStarSolver m p where
  AStarSolver :: (Monad m, Hashable p, Ord p) =>
    { neighbors :: p -> m [p]
    , distance  :: p -> p -> m Int -- for adjacent points only
    , heuristic :: p -> p -> m Int
    , p2i       :: p -> Int
    , i2p       :: Int -> p
    } -> AStarSolver m p

instance (Monad m) => PrimMonad (STT s m) where
  type PrimState (STT s m) = s
  primitive f =
    STT $ \s ->
      case f s of
        (# t, a #) -> return (STTRet t a)
  {-# INLINE primitive #-}

aStarFind :: forall m p . (Monad m, Hashable p, Ord p, Show p) => AStarSolver m p -> p -> p -> (p -> m Bool) -> m [p]
aStarFind solver src dst stopCond = do
  let maxNodesCount = 1000000
  path <- runSTT $ do
    heap <- HPM.new maxNodesCount           :: STT s m (HPM.Heap s Min)
    openList <- HM.new                      :: STT s m (HM.MHashMap s p (p, Int))
    closedList <- HM.newSized maxNodesCount :: STT s m (HM.MHashMap s p p)
    HM.insert openList src (src, 0)
    HPM.unsafePush (mempty :: Min) (p2i solver src) heap
    let aStarFindRec = do
          top' <- HPM.pop heap -- remove the minimum and return
          case top' of
              Nothing -> return []
              Just (_fscore, i) -> do
                let p0 = i2p solver i
                finished <- lift $ stopCond p0
                if finished
                  then do
                    -- HM.insert p0 (weight0 ^. parent) closedList
                    pg' <- HM.lookup openList p0
                    let (parent, _gscore) = fromMaybe (error "[qm|{p0} is not found in openList|]") pg'
                    HM.insert closedList p0 parent
                    backtraceST closedList p0
                  else do
                    pg' <- HM.lookup openList p0
                    let (parent, gscore) = fromMaybe (error "[qm|{p0} is not found in openList|]") pg'
                    HM.insert closedList p0 parent
                    neighbors <- lift $ neighbors solver p0
                    neighPoints <- filterM (\p -> not <$> member closedList p) neighbors
                    forM_ neighPoints $ \np -> do
                      dist <- lift $ distance solver np p0
                      hue <- lift $ heuristic solver np dst
                      let gscoreNp = gscore + dist
                      let fscoreNp = Min (gscore + dist + hue)
                      pg' <- HM.lookup openList np
                      case pg' of
                        Just (parent, gscore) | gscoreNp < gscore -> do
                          -- the neighbour can be reached with smaller cost - change priority
                          -- otherwise don't touch the neighbour, it will be taken by open_list.pop()
                          -- openList .= Q.insert np f1 w1 openList0
                          HPM.push fscoreNp (p2i solver np) heap
                          HM.insert openList np (parent, gscoreNp)
                        Nothing -> do
                          -- the neighbour is new
                          -- openList .= Q.insert np f1 w1 openList0
                          HPM.push fscoreNp (p2i solver np) heap
                          HM.insert openList np (parent, gscoreNp)
                        _ -> return ()
                    aStarFindRec
    aStarFindRec
  return path
  where
    -- member :: forall m p . (PrimMonad m, Hashable p, Ord p, Show p) => p -> m Bool
    member :: (Monad m, Hashable k, Eq k) => HM.MHashMap s k a -> k -> STT s m Bool
    member hm p = do
      v' <- HM.lookup hm p
      return $ isJust v'

backtraceST :: forall m p . (PrimMonad m, Eq p, Hashable p) => HM.MHashMap (PrimState m) p p -> p -> m [p]
backtraceST closedList dst = backtraceRec dst [dst]
  where
    -- we repeatedly lookup for the parent of the current node
    backtraceRec :: (PrimMonad m, Eq p) => p -> [p] -> m [p]
    backtraceRec current acc = do
      parent' <- HM.lookup closedList current
      case parent' of
        Nothing -> return []
        Just parent
          | current == parent -> return acc
        Just parent -> backtraceRec parent (parent : acc)

movePoint :: Point -> Direction -> Point
movePoint (Point i j) d =
  case d of
    U -> Point (i - 1) (j)
    D -> Point (i + 1) (j)
    L -> Point (i) (j - 1)
    R -> Point (i) (j + 1)

-- λ> :rr heapMatchesList $ (\(f,s) -> (Min f, MyElement s)) <$> [(1,4),(2,3),(3,2),(4,1)]
heapMatchesList :: [(Min,MyElement)] -> Bool
heapMatchesList xs' = runIdentity $ do
  let xs = coerce xs' :: [(Min,Int)]
  let xsSet = fmap (\(p,e) -> (e,p)) xs
  let heapRes = runST $ do
        h <- HPM.new 1000
        HPM.pushList xs h
        ij0 <- HPM.pop h
        traceM $ "ij0=" <> show ij0
        HPM.push (Min 2) 4 h
        ij1 <- HPM.pop h
        traceM $ "ij1=" <> show ij1
        ij2 <- HPM.pop h
        traceM $ "ij2=" <> show ij2
        ij3 <- HPM.pop h
        traceM $ "ij3=" <> show ij3
        ij4 <- HPM.pop h
        traceM $ "ij4=" <> show ij4
        HPM.popAll h
  return $ trace [qm| xs={xs}\nheapRes={heapRes}|] $ True
