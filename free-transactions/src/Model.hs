{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Model where

import Control.Monad.Free
import Control.Monad.Free.TH    (makeFree, makeFreeCon)
import Control.Monad.Trans.Free (FreeT)
import Data.Text                (Text)

type Key = Text
type Val = Text

-- GetX, PutX goes into, say, network or filesystem
-- GetDb, PutDb goes into database
-- PutX :: Key -> Val -> next -> OpsF next
-- PutDb :: Key -> Val -> next -> OpsF next
data OpsF n where
  GetX :: Key -> (Val -> n) -> OpsF n
  GetDb :: Key -> (Val -> n) -> OpsF n
  Failure :: n -> OpsF n
  Transact :: Ops a -> (a -> n) -> OpsF n

instance Functor OpsF where
  fmap f (GetX k n) = GetX k (f . n)
  fmap f (GetDb k n) = GetDb k (f . n)
  fmap f (Failure n) = Failure (f n)
  fmap f (Transact block n) = Transact block (f . n)

type Ops = Free OpsF

-- magic
makeFree ''OpsF

data Stuff = Stuff
  { id :: Int
  , name :: Text
  , age :: Int
  } deriving (Show, Eq)
