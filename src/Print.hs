{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Print where
import Prelude hiding (div)
import Expr

e1 :: Expr
e1 = 1 `mul` ((2 `add` ((3 `mul` (4 `mul` 5)) `add` 6)) `mul` (7 `mul` (8 `div` 9)))

e2 :: Expr
e2 = (1 `mul` ((2 `add` ((3 `mul` 4) `mul` 5)) `add` 6)) `mul` ((7 `mul` 8) `div` 9)

e3 :: Expr
e3 = (((1 `mul` ((2 `add` ((3 `mul` 4) `mul` 5)) `add` 6)) `mul` 7) `mul` 8) `div` 9

e4 :: Expr
e4 = 1 `mul` (2 `add` 3) `mul` 4 `mul` (5 `add` 6 `add` 7)

e5 :: Expr
e5 = (3 `div` (-3)) `div` (1 `mul` (-8)) -- => 3 / (-3) / 1 * (-8)

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
  (Add, Add) -> arr regroup
  (Add, Sub) -> arr regroup
  (Mul, Mul) -> arr regroup
  (Mul, Div) -> arr regroup
  _          -> Op o1 (arr a) (arr b)
  where
    regroup = Op o2 (Op o1 a b1) b2

-- We arrange and then pretty print the expression, so given the expression
-- is arranged, we can consider the following cases (in that order!):
-- 1. (((n + e)..)..), n is always unwrapped
-- 2. n is wrapped depeding on if it is negative or positive
showp :: Expr -> String
showp = pp . arr where
  pp (Op o (Nm n) b) = show n ++ " " ++ show o ++ " " ++ pp b
  pp (Nm n) = if n < 0 then "(" ++ show n ++ ")" else show n
  pp (Op Add (Op Add a1 a2) b) =
    pp a1 ++ " " ++ show Add ++ " " ++ pp a2 ++ " " ++ show Add ++ " " ++ pp b
  pp (Op Add (Op Add a1 a2) b) =
    pp a1 ++ " " ++ show Add ++ " " ++ pp a2 ++ " " ++ show Add ++ " " ++ pp b
  pp _ = undefined

pprint :: Expr -> IO ()
pprint e = putStrLn $ show e ++ "\n" ++ showp e
