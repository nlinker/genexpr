{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Print where
import Prelude hiding (div)
import Expr
import Debug.Trace

e1 :: Expr
e1 = 1 `mul` ((2 `add` ((3 `mul` (4 `mul` 5)) `add` 6)) `mul` (7 `mul` (8 `div` 9)))

e2 :: Expr
e2 = (1 `mul` ((2 `add` ((3 `mul` 4) `mul` 5)) `add` 6)) `mul` ((7 `mul` 8) `div` 9)

e3 :: Expr
e3 = (((1 `mul` ((2 `add` ((3 `mul` 4) `mul` 5)) `add` 6)) `mul` 7) `mul` 8) `div` 9

e4 :: Expr
e4 = ((((1 `mul` 2) `mul` 3) `mul` 4) `mul` 5) `mul` 6

-- arrange expression for a _associative_ operation type
-- (therefore ot can be either + or *); examples:
-- (a+(b+c)+d) -> (((a+b)+c)+d), where a, b, c, d are not Add operations
-- (a*(b*c)*d) -> (((a*b)*c)*d), where a, b, c, d are not Mul operations
--    o1                o2
--   / \               / \
--  a  o2      =>     o1 b2
--     / \           / \
--    b1 b2         a  b1
arr :: Expr -> Expr
-- arr a | trace ("arr " ++ show a) False = undefined
arr n@Nm{} = n
arr ex@(Op _o _a@Nm{} _b@Nm{}) = ex
arr (Op  o  a@Op{}  b@Nm{}) = Op o (arr a) b
arr (Op o1 a b@(Op o2 b1 b2)) = case (o1, o2) of
  (Add, Add) -> arr regrouped
  (Add, Sub) -> arr regrouped
  (Mul, Mul) -> arr regrouped
  (Mul, Div) -> arr regrouped
  _          -> Op o1 (arr a) (arr b)
  where
    regrouped = Op o2 (Op o1 a b1) b2

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
