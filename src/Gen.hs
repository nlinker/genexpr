module Gen where

import Opt

data Expr =
  Add Expr Expr |
  Sub Expr Expr |
  Mul Expr Expr |
  Div Expr Expr |
  Num Integer
  deriving (Eq, Show, Ord)

-- the function either evaluates the expression
-- or if the expression is not int-valued, then
evalInt :: Expr -> Maybe Integer
evalInt = undefined
