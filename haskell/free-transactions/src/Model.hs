{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Model where

import Control.Monad            (when)
import Control.Monad.Free
import Control.Monad.Free.TH    (makeFree, makeFreeCon)
import Control.Monad.Trans.Free (FreeT)
import Data.Text                (Text)
import Data.HashMap             (Map, empty, insert, lookup)

type Key = Text
type Val = Text

-- GetX, PutX goes into, say, network or filesystem
-- GetDb, PutDb goes into database
-- PutX :: Key -> Val -> next -> OpsF next
-- PutDb :: Key -> Val -> next -> OpsF next
data OpsF n where
  GetX :: Key -> (Val -> n) -> OpsF n
  GetVal :: Key -> (Val -> n) -> OpsF n
  PutVal :: Key -> Val -> n -> OpsF n
  Failure :: n -> OpsF n
  Transact :: Ops a -> (a -> n) -> OpsF n



instance Functor OpsF where
  fmap f (GetX k n) = GetX k (f . n)
  fmap f (GetVal k n) = GetVal k (f . n)
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

foo :: Ops (Map Key Val)
foo = do
  let k1 = "foo1"
  let k2 = "foo2"
  (vt1, vt2) <- transact $ do
    v1 <- getX k1
    v2 <- getX k2
    when (v1 == "error")
      failure
    return (v1, v2)
  -- get one by one, not in transaction
  vn1 <- getX k1
  vn2 <- getX k2
  -- build result map
  return $
    insert "vt1" vt1 $
    insert "vt2" vt2 $
    insert "vn1" vn1 $
    insert "vn2" vn2 empty
