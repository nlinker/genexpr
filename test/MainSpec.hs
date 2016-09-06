{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MainSpec where

import Test.Hspec
import Test.QuickCheck
import Run

spec :: Spec
spec = do
  describe "Property X" $
    it "merge is ordered" $
      property (prop_Merge)
  describe "read" $ do
    it "is inverse to show" $
      property $ \x -> (read . show) x == (x :: Int)

ordered :: Ord a => [a] -> Bool
ordered []       = True
ordered [x]      = True
ordered (x:y:xs) = x <= y && ordered (y:xs)

prop_Merge :: [Int] -> [Int] -> Property
prop_Merge xs (ys :: [Int]) =
  ordered xs && ordered ys ==>
    --collect (msort [length xs, length ys]) $
    --collect (length xs + length ys) $
    ordered (xs `merge` ys)
