module Check where

import Control.Applicative (liftA2)
import Gen
import Opt

-- isBound, OptN bounds the term value
isBound :: OptN -> Expr -> Bool
isBound (OptN on) (Nm i) = abs i <= toInteger on
isBound optN (Op op e1 e2) = isBound optN e1 && isBound optN e2

-- boundNum
boundNum :: OptN -> Expr -> Maybe Expr
boundNum optN num@(Nm i) =
  let OptN n = optN in
  if abs i <= toInteger n
    then Just num
    else Nothing
boundNum optN (Op op exp1 exp2) = liftA2 (Op op) (boundNum optN exp1) $ boundNum optN exp2

-- true is n/d is an integer
divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

-- eval and check if the expression is integer valued
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
