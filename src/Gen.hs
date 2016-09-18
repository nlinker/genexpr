{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Gen where

-- import Control.Applicative (liftA2)
import Control.Monad.Catch
import Control.Monad.Identity
import Control.Monad.Random
import Data.Typeable (Typeable)
import Prelude hiding (exp)

import Check
import Expr
import Opt

import Debug.Trace

-- genTree construct a tree basing on the input lists
-- given infinite lists with random contents
-- [True, False, False, True  ..] \
-- [Add, Sub, Div, Sub, Mul   ..] |=> Maybe Expr
-- [1, 10, 23, -2, -43, 34, 0 ..] /
-- Nothing is in case the exression cannot be constructed
genTree :: Int -> [Bool] -> [OT] -> [Integer] -> Maybe Expr
genTree 1 _bs _ops (x:_) = return $ Nm x
genTree n (b:bs) (op:ops) (x:xs) = do
  exp <- genTree (n-1) bs ops xs
  -- there is no need to make division by zero
  return $ if op == Div && x == 0
    then Op op (Nm x) exp
    else finalExp exp
  where
    finalExp exp = if b
      then Op op exp (Nm x)
      else Op op (Nm x) exp
genTree _n _bs _ops _xs = Nothing

data GenException =
  ArgumentException |
  ConfigExcepiton
    deriving (Show, Typeable)

instance Exception GenException

-- pure expression generator, which evaluates
-- and generates the expression. It may fail because:
-- 1. There is division by zero
-- 2. There is non-integer division
-- 3. There is not enough items in the lists
-- getT :: (MonadRandom m, MonadThrow m) => Int -> m Expr
generate :: (MonadRandom m) => Options -> m Expr
generate Options{..} =
  let OptN n = optN
      OptL l = optL
  in genT (toInteger n) (toInteger l - 1)
  where
    -- genT is 0 based, but we call them starting from 1
    genT :: (MonadRandom m) => Integer -> Integer -> m Expr
    genT n 0 = Nm <$> getRandomR (-n, n)
    genT n l | l > 0 = do
      i <- getRandomR (0, l - 1)
      e1 <- genT n i
      e2 <- genT n (l - i - 1)
      ot <- getRandom
      return $ Op ot e1 e2
    genT _ _ = error "Wrong argument(s)"
