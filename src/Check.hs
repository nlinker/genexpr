module Check where

import Control.Applicative (liftA2)

import Gen
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
