{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Opt where

import           GHC.Generics       (Generic)
import           WithCli            (HasArguments)

data Options = Options {
    optL :: OptL,
    optN :: OptN,
    optM :: OptM,
    optVersion :: Bool
  }
  deriving (Show, Eq, Generic)

newtype OptN = OptN Int deriving (Eq, Show, HasArguments)
newtype OptM = OptM Int deriving (Eq, Show, HasArguments)
newtype OptL = OptL Int deriving (Eq, Show, HasArguments)

instance HasArguments Options

mkOpt :: Int -> Int -> Int -> Options
mkOpt l n m = Options {
    optL = OptL l,
    optN = OptN n,
    optM = OptM m,
    optVersion = False
  }
