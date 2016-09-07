{-# LANGUAGE ScopedTypeVariables #-}

module MainSpec where

import Test.Hspec
import Test.QuickCheck
import Prelude hiding (div)

import Check
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
