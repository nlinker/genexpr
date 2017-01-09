{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Lib where

import Control.Monad.Free
import Control.Monad.Trans.Free (FreeT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Free.TH  (makeFree)
import Data.ByteString.Lazy   (ByteString)
import Data.Text              (Text)

type Key = Text
type Val = Text
data OpsF a b next = Get Key (Val -> next)
                  | Put Key Val next
                  | Transact (OpsM a b) next
                    deriving (Functor)

type OpsM a = Free (OpsF a m)

-- magic
makeFree ''OpsF

transact1 :: forall (m :: * -> *) a. MonadFree (OpsF a) m => OpsM a a -> m ()
transact1 = undefined

-- run foo in IO
runFoo :: IO ()
runFoo = putStrLn "someFunc"

foo :: OpsM a b
foo = do
  put "foo1" "value1"
  put "foo2" "value2"
  transact $ do
    (t1, t2) <- transact $ do
      v1 <- get "foo1"
      v2 <- get "foo2"
      return (v1, v2)
    transact $ do
      put "foo1" t2
      put "foo2" t1


runOps :: MonadIO m => OpsM a a -> m a
runOps = iterM run where
  run :: MonadIO m => OpsF (m a) -> m a
  run = undefined
