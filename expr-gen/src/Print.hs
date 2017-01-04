{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Print where
import Prelude hiding (div)
import Expr

-- arrange expression for a _associative_ operation type
-- (therefore ot can be either + or *); examples:
-- (a+(b+c)+d) -> (((a+b)+c)+d), where a, b, c, d are not Add operations
-- (a*(b*c)*d) -> (((a*b)*c)*d), where a, b, c, d are not Mul operations
--    o1                o2
--   / \               / \
--  a  o2      =>     o1 b2
--     / \           / \
--    b1 b2         a  b1
arrange :: Expr -> Expr
-- arr a | trace ("arr " ++ show a) False = undefined
arrange n@Nm{} = n
arrange ex@(Op _o _a@Nm{} _b@Nm{}) = ex
arrange (Op  o  a@Op{}  b@Nm{}) = Op o (arrange a) b
arrange (Op o1 a b@(Op o2 b1 b2)) = case (o1, o2) of
  (Add, Add) -> arrange regroup
  (Add, Sub) -> arrange regroup
  (Mul, Mul) -> arrange regroup
  (Mul, Div) -> arrange regroup
  _          -> Op o1 (arrange a) (arrange b)
  where
    regroup = Op o2 (Op o1 a b1) b2

-- We arrange and then pretty print the expression, so given the expression
-- is arranged, we can consider the following cases (in that order!):
-- 1. (((n + e)..)..), n is always unwrapped
-- 2. n is wrapped depeding on if it is negative or positive
pp :: Expr -> String
pp = pp0 False . arrange where

  -- wrap expression in parens when the expression is
  -- the expected operation. E.g. in case of this tree
  --    *
  --   / \     when ? = + or -, b1 ? b2 should be wrapped:
  --  a   ?      a * (b1 + b2)
  --     / \   when ? = * or /, b1 ? b2 should not:
  --    b1 b2    a * b1 / b2
  wrap :: Bool -> Expr -> [OT] -> String
  wrap f e ots = case e of
    Nm n     -> pp0 f (Nm n)
    Op o _ _ -> if o `elem` ots
      then "(" ++ pp0 f e ++ ")"
      else pp0 f e

  -- special case 1 + (((-2) * 3) * 4)
  -- the sub-expression is not wrapped, but the leftest number in
  -- the sub-expression should be wrapped if it is negantive
  -- so 1 + (((-2) * 3) * 4) => 1 + (-2) * 3 * 4
  -- or 1 + (((-2) / 3) - 4) => 1 + (-2) / 3 - 4
  wrapSpecial :: Expr -> [OT] -> String
  wrapSpecial e ots = case e of
    Op Mul _ _ -> wrap True e ots
    Op Div _ _ -> wrap True e ots
    _          -> wrap False e ots

  pp0 :: Bool -> Expr -> String
  -- pp f a | trace ("pp " ++ show a) False = undefined
  pp0 _ (Nm n) = if n < 0 then "(" ++ show n ++ ")" else show n
  pp0 f (Op o a@(Nm na) b@(Nm _)) =
    if f -- wrap the leftest negative number, see wrapSpecial
      then pp0 f a ++ " " ++ show o ++ " " ++ pp0 f b
      else show na ++ " " ++ show o ++ " " ++ pp0 f b
  pp0 f (Op Add a b) =
    let x = wrap f a []
        y = wrapSpecial b []
    in x ++ " " ++ show Add ++ " " ++ y
  pp0 f (Op Sub a b) =
    let x = wrap f a []
        y = wrapSpecial b [Add, Sub]
    in x ++ " " ++ show Sub ++ " " ++ y
  pp0 f (Op Mul a b) =
    let x = wrap f a [Add, Sub]
        y = wrap f b [Add, Sub]
    in x ++ " " ++ show Mul ++ " " ++ y
  pp0 f (Op Div a b) =
    let x = wrap f a [Add, Sub]
        y = wrap f b [Add, Sub, Mul, Div]
    in x ++ " " ++ show Div ++ " " ++ y

show2 :: Expr -> String
show2 e = show e ++ "\n" ++ pp e
