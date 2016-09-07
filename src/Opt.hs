{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Opt where

import           GHC.Generics       (Generic)
import           WithCli            (HasArguments)

data Options = Options {
    optN :: OptN,
    optM :: OptM,
    optL :: OptL,
    optVersion :: Bool
  }
  deriving (Show, Eq, Generic)

newtype OptN = OptN Int deriving (Eq, Show, HasArguments)
newtype OptM = OptM Int deriving (Eq, Show, HasArguments)
newtype OptL = OptL Int deriving (Eq, Show, HasArguments)

instance HasArguments Options
