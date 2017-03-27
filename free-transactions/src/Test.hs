{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}

module Test where

import Prelude hiding           (log)
import Control.Monad            (when)
import Control.Monad.IO.Class   (MonadIO, liftIO)
import Control.Monad.Free
import Control.Monad.Free.TH    (makeFree, makeFreeCon)
import Control.Monad.Trans.Free (FreeT)
import Data.Text                (Text, pack)
import Data.HashMap             (Map, empty, insert, lookup)
import Data.Function            (id)
import Control.Monad.Except     (MonadError, runExceptT, catchError, throwError)
import Control.Exception        (IOException(..), SomeException(..), Exception, throw)
import Data.Typeable            (Typeable)

type Key = Text
type Val = Text

data TestF n where
  Log :: Text -> n -> TestF n
  Get :: Key -> (Val -> n) -> TestF n
  Put :: Key -> Val -> n -> TestF n
  Transact :: TestFree a -> (a -> n) -> TestF n
  Rollback :: n -> TestF n
  Commit :: n -> TestF n

type TestFree = Free TestF

instance Functor TestF where
  fmap f (Log msg n) = Log msg (f n)
  fmap f (Get k r) = Get k (f . r)
  fmap f (Put k v n) = Put k v (f n)
  fmap f (Transact block r) = Transact block (f . r)
  fmap f (Rollback n) = Rollback (f n)
  fmap f (Commit n) = Commit (f n)

makeFree ''TestF

foo :: (MonadFree TestF m) => m ()
foo = do
  x <- get "k1"
  put "k2" x
  z <- transact $ do
    y <- get "k3"
    log y
    when (y == "42")
      rollback
    put "k4" y
    return (42 :: Int)
  log $ pack $ show z

--log :: (MonadFree TestF m) => Text -> m ()
--log msg = liftF $ Log msg ()
--
--get :: (MonadFree TestF m) => Key -> m Val
--get k = liftF $ Get k id
--
--put :: (MonadFree TestF m) => Key -> Val -> m ()
--put k v = liftF $ Put k v ()
--
--transact :: (MonadFree TestF m) => TestFree a -> m a
--transact block = liftF $ Transact block id
--
--rollback :: (MonadFree TestF m) => m ()
--rollback = liftF $ Rollback ()
--
--commit :: (MonadFree TestF m) => m ()
--commit = liftF $ Commit ()
