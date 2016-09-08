{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Gen where

import Control.Applicative (liftA2)
import Prelude hiding (exp)

-- the expression is either number or some operation on two args
data Expr =
  Op OT Expr Expr |
  Nm Integer
  deriving (Eq, Show, Ord)

-- operation type
data OT = Add | Sub | Mul | Div
  deriving (Eq, Show, Ord)

-- the class and functions to enable build expressions
-- more naturally, like x = 1 `add` (2 `mul` 3)
class ToExpr a where
  toExpr :: a -> Expr

instance ToExpr Expr where
  toExpr = id

instance (Integral n) => ToExpr n where
  toExpr = Nm . toInteger

add :: (ToExpr a, ToExpr b) => a -> b -> Expr
add x y = Op Add (toExpr x) (toExpr y)

sub :: (ToExpr a, ToExpr b) => a -> b -> Expr
sub x y = Op Sub (toExpr x) (toExpr y)

mul :: (ToExpr a, ToExpr b) => a -> b -> Expr
mul x y = Op Mul (toExpr x) (toExpr y)

div :: (ToExpr a, ToExpr b) => a -> b -> Expr
div x y = Op Div (toExpr x) (toExpr y)

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
    n1 <- intVal exp1
    n2 <- intVal exp2
    if n2 /= 0 && n2 `divides` n1
      then Just (n1 `Prelude.div` n2)
      else Nothing

-- true is n/d is an integer
divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0
