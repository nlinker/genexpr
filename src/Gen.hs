{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

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

-- arrange expression: (2+(3+4)+5) -> (((2+3)+4)+5)
arr :: Expr -> Expr
arr n@Nm{} = n
arr op@(Op ot n1@Nm{} n2@Nm{}) = op
arr op@(Op ot e1@Op{} n2@Nm{}) = Op ot (arr e1) n2
arr op@(Op ot e1 e2) =
  let e1a = arr e1
      e2a = arr e2
      Op o2 e2' n2 = e2a
      Op ol l1  l2 = low e2a
      eot = Op ot e1a l1
  in Op ol eot l2

low :: Expr -> Expr
low _n@Nm{} = error "whoa"
low  op@(Op _ot _n1@Nm{} _e2) = op
low _op@(Op _ot  e1@Op{} _e2) = low e1


eg :: Expr
eg = 1 `add` (2 `add` 3) `add` (4 `add` 5)

-- convE2EP guarantees:
-- 1. NegP n is constructed only with n < 0
-- 2. PosP n is constructed only with n >= 0
-- 3. SumP is constructed with Add or Sub
-- 4. ProdP is constructed with Mul or Div
convE2EP :: Expr -> ExprP
convE2EP (Nm n) = NumP n
convE2EP (Op ot e1 e2) =
  case ot of
    x | x == Add || x == Sub -> SumP ot (convE2EP e1) (convE2EP e2)
    x | x == Mul || x == Div -> ProdP ot (convE2EP e1) (convE2EP e2)
    _ -> error "convE2EP: impossible match"

pprint :: Expr -> IO ()
pprint e = putStrLn $ show e ++ "\n" ++ (show . convE2EP $ e)
