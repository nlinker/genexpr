{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module SimpleSpec where

import           Control.Exception (evaluate)
import           Test.Hspec

spec :: Spec
spec =
  describe "Prelude.read" $ do
    it "can parse integers" $ --do
      read "10" `shouldBe` (10 :: Int)
    it "can parse floating-point numbers" $ --do
      read "2.5" `shouldBe` (2.5 :: Float)
    it "throws an exception if used with an empty list" $ --do
      evaluate (head []) `shouldThrow` anyException
    it "calculate" $
      split 13 `shouldBe` ([13,1,13,3,12,5,10,7],[8,9,6,11,4,13,2,13])
      split 16 `shouldBe` ([16,1,14,3,12,5,10,7],[8,9,6,11,4,13,2,15])
      split 12 `shouldBe` ([12,1,10,3,8,5],[6,7,4,9,2,11])
