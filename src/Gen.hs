{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Gen where

import Prelude hiding (div)
import Opt

-- the expression is either number or some operation on two args
data Expr =
  Op OT Expr Expr |
  Nm Integer
  deriving (Eq, Show, Ord)

-- operation type
data OT = Add | Sub | Mul | Div
  deriving (Eq, Show, Ord)

-- the class and functions to enable build expressions
-- more naturally, like 1 `add` (2 `mul` 3)
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

-- the function either evaluates the expression
-- or if the expression is not int-valued, then
evalInt :: Expr -> Maybe Integer
evalInt = undefined

-- genTree having a list of operations and integers
-- construct a tree
genTree :: [OT] -> [Integer] -> Expr
genTree ops xs = undefined
