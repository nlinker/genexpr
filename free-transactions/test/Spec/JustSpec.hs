{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}

module Spec.JustSpec where

import           Control.Exception          (evaluate)
import           GHC.Generics               (Generic)
import           Test.Hspec                 (hspec)

import qualified Data.ByteString.Lazy       as B

-- import           Control.Monad.Base     (MonadBase)
-- import           Control.Monad.IO.Class (MonadIO, liftIO)
-- import           Data.Aeson.Encode
-- import           Vertigo.Types.UserId
-- import           Vertigo.Web

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Prelude.read" $ do
    it "can parse integers" $ --do
      read "10" `shouldBe` (10 :: Int)
    it "can parse floating-point numbers" $ --do
      read "2.5" `shouldBe` (2.5 :: Float)
    it "throws an exception if used with an empty list" $ --do
      evaluate (head []) `shouldThrow` anyException
