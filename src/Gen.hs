{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Gen where

-- import Control.Applicative (liftA2)
import Control.Monad.Catch
import Control.Monad.Identity
import Control.Monad.Random
import Data.Typeable (Typeable)
import Prelude hiding (exp)

import Expr
import Opt

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
  ConfigException
    deriving (Show, Typeable)

instance Exception GenException

-- Almost unrestricted expression generator, that provides only that
-- 1. The expression tree is random
-- 2. The number of literals is L
-- 3. Each literal n is bounded -N <= n <= N
--
-- However it is NOT guaranteed that the expression
-- 1. is valid and integer
-- 2. evaluates to number m so that -M <= m <= M
--
-- see the function `generate` for the complete
generateU :: (MonadRandom m) => Options -> m Expr
generateU Options{..} =
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
