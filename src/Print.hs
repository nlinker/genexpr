{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Print where

import Expr

e1 :: Expr
e1 = 1 `mul` (2 `add` 3) `mul` (4 `mul` (5 `add` (6 `add` 7)))

arrA :: Expr -> Expr
arrA = arr Add

arrM :: Expr -> Expr
arrM = arr Mul

-- arrange expression for a _associative_ operation type
-- (therefore ot can be either + or *); examples:
-- (a+(b+c)+d) -> (((a+b)+c)+d), where a, b, c, d are not Add operations
-- (a*(b*c)*d) -> (((a*b)*c)*d), where a, b, c, d are not Mul operations
arr :: OT -> Expr -> Expr
arr _ot n@Nm{} = n
arr _ot ex@(Op _o _n1@Nm{} _n2@Nm{}) = ex
arr ot ex@(Op  o e1@Op{} n2@Nm{}) =
  if ot == o
    then Op o (arr ot e1) n2
    else ex
arr ot ex@(Op  o e1 e2) =
  if o == ot
    then union o (arr ot e1) (arr ot e2)
    else ex

-- union of two _arranged_ expressions
--    o                 o1
--   / \               / \
--  e1  o1  \    =>   o   y
--     / \  > e2     / \
--    x   y /       e1  x
union :: OT -> Expr -> Expr -> Expr
union o e1 e2 = case e2 of
  y@Nm{}    -> Op o e1 y
  -- Op o1 x y@Nm{} -> Op o1 (union o e1 x) y
  Op o1 x y -> if o == o1
    then Op o1 (union o e1 x) y
    else Op o e1 e2

-- convE2EP guarantees:
-- 1. NegP n is constructed only with n < 0
-- 2. PosP n is constructed only with n >= 0
-- 3. SumP is constructed with Add or Sub
-- 4. ProdP is constructed with Mul or Div
convE2EP :: Expr -> ExprP
convE2EP (Nm n) = NumP n
convE2EP (Op ot e1 e2) =
  case ot of
    x | x == Add || x == Sub -> SumP ot (convE2EP e1) (convE2EP e2)
    x | x == Mul || x == Div -> ProdP ot (convE2EP e1) (convE2EP e2)
    _ -> error "convE2EP: impossible match"

pprint :: Expr -> IO ()
pprint e = putStrLn $ show e ++ "\n" ++ (show . convE2EP $ e)
