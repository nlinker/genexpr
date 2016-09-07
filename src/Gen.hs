module Gen where

import Control.Applicative (liftA2)
import Opt

data Expr =
  Add Expr Expr |
  Sub Expr Expr |
  Mul Expr Expr |
  Div Expr Expr |
  Num Integer
  deriving (Eq, Show, Ord)

-- bound
boundNum :: OptN -> Expr -> Maybe Expr
boundNum (OptN on) n@(Num i) =
  if abs i <= toInteger on
    then Just n
    else Nothing
boundNum optN (Add exp1 exp2) = liftA2 Add (boundNum optN exp1) $ boundNum optN exp2
boundNum optN (Sub exp1 exp2) = liftA2 Sub (boundNum optN exp1) $ boundNum optN exp2
boundNum optN (Mul exp1 exp2) = liftA2 Mul (boundNum optN exp1) $ boundNum optN exp2
boundNum optN (Div exp1 exp2) = liftA2 Div (boundNum optN exp1) $ boundNum optN exp2

-- true is n/d is an integer
divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

-- integer valued expression
intVal :: Expr -> Maybe Integer
intVal (Add exp1 exp2) = liftA2 (+) (intVal exp1) $ intVal exp2
intVal (Sub exp1 exp2) = liftA2 (-) (intVal exp1) $ intVal exp2
intVal (Mul exp1 exp2) = liftA2 (*) (intVal exp1) $ intVal exp2
intVal (Div exp1 exp2) = do
  n1 <- intVal exp1
  n2 <- intVal exp2
  if n2 /= 0 && n2 `divides` n1
    then Just (n1 `div` n2)
    else Nothing
intVal (Num n) = return n


-- the function either evaluates the expression
-- or if the expression is not int-valued, then
evalInt :: Expr -> Maybe Integer
evalInt = undefined
