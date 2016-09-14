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
  deriving (Eq, Show, Ord)

-- operation type
data OT = Add | Sub | Mul | Div
  deriving (Eq, Show, Ord)

instance Random OT where
  randomR (o1, o2) g = convertTo `first` randomR (convertFrom o1, convertFrom o2) g
  random g = convertTo `first` randomR (0, 3) g

-- helper methods to define Random instance over OT
ots :: [(OT, Int)]
ots = [(Add, 0), (Sub, 1), (Mul, 2), (Div, 3)]

convertTo :: Int -> OT
convertTo i = fst . head $ filter (\(_, x) -> x == i) ots

convertFrom :: OT -> Int
convertFrom ot = snd . head $ filter (\(x, _) -> x == ot) ots


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

-- operators to move even further
-- xxx = 1.+.2.-.3.*.4./.5
-- (.+.) :: (ToExpr a, ToExpr b) => a -> b -> Expr
-- (.+.) = add
-- (.-.) :: (ToExpr a, ToExpr b) => a -> b -> Expr
-- (.-.) = sub
-- (.*.) :: (ToExpr a, ToExpr b) => a -> b -> Expr
-- (.*.) = mul
-- (./.) :: (ToExpr a, ToExpr b) => a -> b -> Expr
-- (./.) = div
