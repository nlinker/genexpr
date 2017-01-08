{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
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
data Ops next = Get Key (Maybe Val -> next)
                  | Put Key Val next
                  | Transact (OpsM ()) next
                    deriving (Functor)

type OpsM = Free Ops

-- from https://github.com/dhess/free-experiments/blob/3e878ec743df9045ea9b76c85f8184e38d0807e2/src/RetryTransTH.hs
data RetryF m next where
  Output    :: String -> next -> RetryF m next
  Input     :: Read a => (a -> next) -> RetryF m next
  WithRetry :: (RetryT m) m a -> (a -> next) -> RetryF m next
  Retry     :: RetryF m next

-- data (Read a) => RetryF m a next =  Output String next
--                                   | Input (a -> next)
--                                   | WithRetry ((RetryT m) m a) (a -> next)
--                                   | Retry

-- type RetryT m = FreeT (RetryF m a)
type RetryT m = FreeT (RetryF m)

instance Functor (RetryF m) where
  fmap f (Output s x) = Output s (f x)
  fmap f (Input g) = Input (f . g)
  fmap f (WithRetry block g) = WithRetry block (f . g)
  fmap _ Retry = Retry

-- magic
makeFree ''Ops

-- run foo in IO
runFoo :: IO ()
runFoo = putStrLn "someFunc"

foo :: OpsM ()
foo = do
  put "foo1" "value1"
  put "foo2" "value2"
  transact $ do
    (t1, t2) <- transact $ do
      v1 <- get "foo1"
      v2 <- get "foo1"
      return (v1, v2)
    transact $ do
      put "foo1" v2
      put "foo2" v1


runOps :: MonadIO m => OpsM a -> m a
runOps = iterM run where
  run :: MonadIO m => Ops (m a) -> m a
  run = undefined
