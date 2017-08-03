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
