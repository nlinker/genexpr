{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-
TODO don't know what to do
Defaulting the following constraint(s) to type ‘Integer’
  (Integral a0)
    arising from a use of ‘div’ at test/MainSpec.hs:41:18-22
  (Num a0) arising from the literal ‘1’ at test/MainSpec.hs:41:16
In the first argument of ‘add’, namely ‘(1 `div` 2)’
In the expression: (1 `div` 2) `add` 3
In an equation for ‘e’: e = (1 `div` 2) `add` 3
-}

module MainSpec where

import Test.Hspec
import Test.QuickCheck
import Prelude hiding (div, exp)

import Check
import Expr
import Opt
import Gen
import Run

spec :: Spec
spec = do
  describe "boundNum" $ do
    it "returns the same if bounds are ok" $ do
      let e1 = (1 `mul` 2) `add` 3
      let on = OptN 10
      boundNum on e1 `shouldBe` Just e1
    it "returns Noting when a leaf is too high" $ do
      let e1 = (11 `mul` 2) `add` 3
      let on = OptN 10
      boundNum on e1 `shouldBe` Nothing
    it "returns Noting when a leaf is too low" $ do
      let e1 = ((-11) `mul` 2) `add` 3
      let on = OptN 10
      boundNum on e1 `shouldBe` Nothing
  describe "intVal" $ do
    it "evaluates normally" $ do
      let e = (1 `mul` 2) `add` 3
      intVal e `shouldBe` Just 5
    it "even with division" $ do
      let e = (2 `div` 1) `add` 3
      intVal e `shouldBe` Just 5
    it "floating division results to Nothing" $ do
      let e = (1 `div` 2) `add` 3
      intVal e `shouldBe` Nothing
    it "divide by zero results to Nothing" $ do
      let e = (1 `div` 0) `add` 3
      intVal e `shouldBe` Nothing
  describe "genTree" $ do
    it "normal left tree" $ do
      let bs = const False <$> [1..]
      let ops = const Add <$> [1..]
      let xs = [1..]
      let tree = genTree 4 bs ops xs
      let expt = Just $ 1 `add` (2 `add` (3 `add` 4))
      tree `shouldBe` expt
    it "normal right tree" $ do
      let bs = const True <$> [1..]
      let ops = const Add <$> [1..]
      let xs = [1..]
      let tree = genTree 4 bs ops xs
      let expt = Just $ 4 `add` 3 `add` 2 `add` 1
      tree `shouldBe` expt
    it "div by zero" $ do
      let bs = const False <$> [1..]
      let ops = const Div <$> [1..]
      let xs = [1,2,3,0]
      let tree = genTree 4 bs ops xs
      let expt = Nothing
      tree `shouldBe` expt

  describe "properties" $ do
    it "is inverse to show" $
      property $ \x -> (read . show) x == (x :: Int)
    it "another property test" $
      property $ forAll (genWithin 10) $ \x -> x < 11 && x > negate 11

genWithin :: Int -> Gen Int
genWithin n = (arbitrary :: Gen Int) `suchThat` (\x -> abs x <= n)

ordered :: Ord a => [a] -> Bool
ordered []       = True
ordered [_]      = True
ordered (x:y:xs) = x <= y && ordered (y:xs)

prop_Merge :: [Int] -> [Int] -> Property
prop_Merge xs (ys :: [Int]) =
  ordered xs && ordered ys ==>
    --collect (msort [length xs, length ys]) $
    --collect (length xs + length ys) $
    ordered (xs `merge` ys)

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

ess :: [Expr]
ess = [((-1) `o2` (-2)) `o1` ((-3) `o3` (-4)) |
        o1 <- ops, o2 <- ops, o3 <- ops]
  where
    ops :: (ToExpr a, ToExpr b) => [a -> b -> Expr]
    ops = [add, sub, mul, div]
