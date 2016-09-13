{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Gen where

import Control.Applicative (liftA2)
import Control.Monad.Catch
import Control.Monad.Identity
import Data.Typeable (Typeable)
import Prelude hiding (exp)
import Expr

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
getT :: MonadThrow m => Int -> [Bool] -> [OT] -> [Integer] -> m Expr
getT _a _b _c _d = throwM ArgumentException

-- eval and check if the expression is integer valued
-- the function either evaluates the expression
-- or if the expression is not int-valued, then Nothing
intVal :: Expr -> Maybe Integer
intVal (Nm n) = return n
intVal (Op op exp1 exp2) = case op of
  Add -> liftA2 (+) (intVal exp1) $ intVal exp2
  Sub -> liftA2 (-) (intVal exp1) $ intVal exp2
  Mul -> liftA2 (*) (intVal exp1) $ intVal exp2
  Div -> do
    n <- intVal exp1
    d <- intVal exp2
    if n `divOk` d
      then Just (n `Prelude.div` d)
      else Nothing

-- true is n/d is an integer and d is nonzero
divOk :: Integer -> Integer -> Bool
divOk n d = d /= 0 && rem n d == 0
