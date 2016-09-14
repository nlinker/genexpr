module Check where

import Control.Applicative (liftA2)

import Opt
import Expr

-- isBound, OptN bounds the term value
isBound :: OptN -> Expr -> Bool
isBound (OptN on) (Nm i) = abs i <= toInteger on
isBound optN (Op _op e1 e2) = isBound optN e1 && isBound optN e2

-- boundNum
boundNum :: OptN -> Expr -> Maybe Expr
boundNum optN num@(Nm i) =
  let OptN n = optN in
  if abs i <= toInteger n
    then Just num
    else Nothing
boundNum optN (Op op exp1 exp2) = liftA2 (Op op) (boundNum optN exp1) $ boundNum optN exp2

-- true is n/d is an integer and d is nonzero
divOk :: Integer -> Integer -> Bool
divOk n d = d /= 0 && rem n d == 0

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
