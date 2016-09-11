{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}

module Expr where

import Prelude hiding (div)

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
