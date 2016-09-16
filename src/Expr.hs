{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}

module Expr where

import Control.Arrow
import Control.Monad.Random
import Prelude hiding (div)

-- the expression is either number or some operation on two args
data Expr =
  Op OT Expr Expr |
  Nm Integer
  deriving (Eq)

-- operation type
data OT = Add | Sub | Mul | Div
  deriving (Eq, Ord)

-- special variant of the expression above to be able to pretty
-- print the expressiion (= without redundant parens).
data ExprP =
  NegP Integer |
  PosP Integer |
  SumP  OT ExprP ExprP | -- lower priority operations: Add or Sub
  ProdP OT ExprP ExprP   -- higher priority operations: Mul or Div
  deriving (Eq)

---------------
-- INSTANCES --

-- show expression verbatim in the convential form
-- pretty printing is done via (show . convE2EP)
instance Show Expr where
  show (Nm n) = if n < 0 then "(" ++ show n ++ ")" else show n
  show (Op ot e1 e2) = "(" ++ show e1 ++ " " ++ show ot ++ " " ++ show e2 ++ ")"

instance Show OT where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

-- rules:
-- 0. Positive literals aren't wrapped: 3 + 4
-- 1. Negative literals aren't wrapped if they are at the
--    beginning of the expression or right after the paren:
--      ((-3) + 4) => -3 + 4
--      (2 * ((-3) + 4)) => 2 * (-3 + 4)
-- 2. Sums are flattened:
--      ((3 + (-4)) + ((-5) + 6)) => 3 + (-4) + (-5) + 6
-- 3. Products are flattened:
--      ((3 * (-4)) * ((-5) * 6)) => 3 * (-4) * (-5) * 6
-- 4. Precedence rules are applied:
--      ((1 * 2) + (3 / 4)) => 1 * 2 + 3 / 4
-- 5. Products before division aren't put in parens:
--      (3 * (-4)) / 3 => 3 * (-4) / 3
-- 6.
instance Show ExprP where
  show e = pp [] e
    where
      pp :: [Bool] -> ExprP -> String
      pp xs (SumP ot (NegP n) e2) = show n ++ " " ++ show ot ++ " " ++ pp xs e2
      pp xs (ProdP ot (NegP n) e2) = show n ++ " " ++ show ot ++ " " ++ pp xs e2
      pp xs (NegP n) = "(" ++ show n ++ ")"
      pp xs (SumP ot e1 e2) = pp xs e1 ++ " " ++ show ot ++ " " ++ pp xs e2
      pp xs (ProdP ot e1 e2) = pp xs e1 ++ " " ++ show ot ++ " " ++ pp xs e2


instance Random OT where
  randomR (o1, o2) g = convertTo `first` randomR (convertFrom o1, convertFrom o2) g
  random g = convertTo `first` randomR (0, 3) g

-- the class and functions to enable build expressions
-- more naturally, like x = 1 `add` (2 `mul` 3)
-- see funcions add, sub, mul, div
class ToExpr a where
  toExpr :: a -> Expr

instance ToExpr Expr where
  toExpr = id

instance (Integral n) => ToExpr n where
  toExpr = Nm . toInteger

-- helper methods to define Random instance over OT
ots :: [(OT, Int)]
ots = [(Add, 0), (Sub, 1), (Mul, 2), (Div, 3)]

convertTo :: Int -> OT
convertTo i = fst . head $ filter (\(_, x) -> x == i) ots

convertFrom :: OT -> Int
convertFrom ot = snd . head $ filter (\(x, _) -> x == ot) ots

add :: (ToExpr a, ToExpr b) => a -> b -> Expr
add x y = Op Add (toExpr x) (toExpr y)

sub :: (ToExpr a, ToExpr b) => a -> b -> Expr
sub x y = Op Sub (toExpr x) (toExpr y)

mul :: (ToExpr a, ToExpr b) => a -> b -> Expr
mul x y = Op Mul (toExpr x) (toExpr y)

div :: (ToExpr a, ToExpr b) => a -> b -> Expr
div x y = Op Div (toExpr x) (toExpr y)

-- operators to sugar it even further
-- xxx = 1.+.2.-.3.*.4./.5
-- (.+.) :: (ToExpr a, ToExpr b) => a -> b -> Expr
-- (.+.) = add
-- (.-.) :: (ToExpr a, ToExpr b) => a -> b -> Expr
-- (.-.) = sub
-- (.*.) :: (ToExpr a, ToExpr b) => a -> b -> Expr
-- (.*.) = mul
-- (./.) :: (ToExpr a, ToExpr b) => a -> b -> Expr
-- (./.) = div
