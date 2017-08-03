# How to make transactions in Free monads

Typical DSL algebra looks like this:
```haskell
type Action n v i = FreeT (ActionF n v i)

data ActionF n v i a =
  NewNode v (n -> a) |
  GetValue n (v -> a) |
  SetValue n v a |
  GetRoot (n -> a) |
  GetTargets n i ([n] -> a) |
  AddTarget n n a |
  RemoveTarget n n a |
  Remove n a |
  GetStats ((Int, Int, Int) -> a)
  deriving (Functor)

makeFree ''ActionF
```
But if we extend this DSL with `Transact Action` variant, it does not typecheck.

There is a variant to use GADTs to encode the algebra:
```haskell
-- from https://github.com/dhess/free-experiments/blob/3e878ec743df9045ea9b76c85f8184e38d0807e2/src/RetryTransTH.hs
data RetryF m next where
  Output    :: String -> next -> RetryF m next
  Input     :: Read a => (a -> next) -> RetryF m next
  WithRetry :: (RetryT m) m a -> (a -> next) -> RetryF m next
  Retry     :: RetryF m next

-- data (Read a) => RetryF m a next = Output String next
--                                  | Input (a -> next)
--                                  | WithRetry ((RetryT m) m a) (a -> next)
--                                  | Retry

-- type RetryT m = FreeT (RetryF m a)
type RetryT m = FreeT (RetryF m)

instance Functor (RetryF m) where
  fmap f (Output s x) = Output s (f x)
  fmap f (Input g) = Input (f . g)
  fmap f (WithRetry block g) = WithRetry block (f . g)
  fmap _ Retry = Retry
```
which works and the example is here.

## Another attempt to transactions

```
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module Test where

import Prelude hiding           (log)
import Control.Monad            (when)
import Control.Monad.Free
import Control.Monad.Free.TH    (makeFree, makeFreeCon)
import Control.Monad.Trans.Free (FreeT)
import Data.Text                (Text, pack)
import Data.HashMap             (Map, empty, insert, lookup)
import Data.Function            (id)

type Key = Text
type Val = Text

type TestFree a = Free (TestF a)

data TestF a n =
    Log Text n
  | Get Key (Val -> n)
  | Put Key Val n
  | Transact (TestFree a n) (a -> n)
  | Rollback n
  | Commit n

instance Functor (TestF a) where
  fmap f (Log msg n) = Log msg (f n)
  fmap f (Get k r) = Get k (f . r)
  fmap f (Put k v n) = Put k v (f n)
  fmap f (Transact block r) = Transact block (f . r) -- ERROR
  fmap f (Rollback n) = Rollback (f n)
  fmap f (Commit n) = Commit (f n)

--makeFree ''TestF

log :: (MonadFree (TestF a) m) => Text -> m ()
log msg = liftF $ Log msg ()

get :: (MonadFree (TestF a) m) => Key -> m Val
get k = liftF $ Get k id

put :: (MonadFree (TestF a) m) => Key -> Val -> m ()
put k v = liftF $ Put k v ()

transact :: (MonadFree (TestF a) m) => TestFree a a -> m a
transact block = liftF $ Transact block id

rollback :: (MonadFree (TestF a) m) => m ()
rollback = liftF $ Rollback ()

commit :: (MonadFree (TestF a) m) => m ()
commit = liftF $ Commit ()

foo :: (MonadFree (TestF a) m) => m ()
foo = do
  x <- get "k1"
  put "k2" x
  z <- transact $ do -- ERROR
    y <- get "k3"
    when (y == "42")
      rollback
    put "k4" y
    return (42 :: Int)
  log $ pack $ show z
```