{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Print where
import Prelude hiding (div)
import Expr
import Debug.Trace

e1 :: Expr
e1 = 1 `mul` ((2 `add` ((3 `mul` (4 `mul` 5)) `add` 6)) `mul` (7 `mul` (8 `div` 9)))

e2 :: Expr
e2 = (((1 `mul` ((2 `add` ((3 `mul` 4) `mul` 5)) `add` 6)) `mul` 7) `mul` 8) `div` 9
--e4 = (-1) `mul` ((-2) `add` (-3)) `mul` (-4) `mul` ((-5) `add` (-6) `add` (-7))

e3 :: Expr
e3 = (3 `div` (-3)) `div` (1 `mul` (-8)) -- => 3 / (-3) / 1 * (-8)

es :: [Expr]
es = [
  (-1) `sub` ((-2) `mul` (-3)),
  (-1) `sub` ((-2) `div` (-3)),
  ((-1) `mul` (-2)) `add` ((-3) `mul` (-4)),
  ((-1) `mul` (-2)) `sub` ((-3) `mul` (-4)),
  ((-1) `add` (-2)) `mul` ((-3) `add` (-4)),
  ((-1) `sub` (-2)) `sub` ((-3) `sub` (-4))
  ]


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
showp = pp False . arr where

  wrapF = wrap False
  wrapT = wrap True

  wrap :: Bool -> Expr -> [OT] -> String
  wrap f e ots = case e of
    n@Nm{}   -> pp f n
    Op o _ _ -> if o `elem` ots
      then "(" ++ pp f e ++ ")"
      else pp f e

  pp :: Bool -> Expr -> String
  --pp f a | trace ("pp " ++ show a) False = undefined
  pp _ (Nm n) = if n < 0 then "(" ++ show n ++ ")" else show n
  pp f (Op o a@(Nm na) b@(Nm _)) =
    if f -- wrap the leftest negative number
      then pp f a ++ " " ++ show o ++ " " ++ pp f b
      else show na ++ " " ++ show o ++ " " ++ pp f b
  pp f (Op Add a b) =
    let x = wrap f a []
        -- special case 1 + (((-2) * 3) * 4)
        -- the subexpression is not wrapped, but the leftest number in
        -- the subexpression should be wrapped if it is negantive
        -- so 1 + (((-2) * 3) * 4) => 1 + (-2) * 3 * 4
        -- or 1 + (((-2) / 3) - 4) => 1 + (-2) / 3 - 4
        y = case b of
          Op o _ _ | o `elem` [Mul, Div] -> wrapT b []
          _                              -> wrapF b []
    in x ++ " " ++ show Add ++ " " ++ y
  pp f (Op Sub a b) =
    let x = wrap f a []
        y = case b of
          Op o _ _ | o `elem` [Mul, Div] -> wrapT b [Add, Sub]
          _                              -> wrapF b [Add, Sub]
    in x ++ " " ++ show Sub ++ " " ++ y
  pp f (Op Mul a b) =
    let x = wrap f a [Add, Sub]
        y = wrap f b [Add, Sub]
    in x ++ " " ++ show Mul ++ " " ++ y
  pp f (Op Div a b) =
    let x = wrap f a [Add, Sub]
        y = wrap f b [Add, Sub, Mul, Div]
    in x ++ " " ++ show Div ++ " " ++ y

pprint :: Expr -> IO ()
pprint e = putStrLn $ show e ++ "\n" ++ showp e

pprints :: [Expr] -> IO ()
pprints = mapM_ pprint
