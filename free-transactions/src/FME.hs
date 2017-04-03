{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}

module FME where

import Prelude hiding           (log)
import Control.Monad            (when)
import Control.Monad.IO.Class   (MonadIO, liftIO)
import Control.Monad.Except     (ExceptT(..), MonadTrans, lift)
import Control.Monad.Free
import Control.Monad.Free.TH    (makeFree, makeFreeCon)
import Control.Monad.Trans.Free (FreeT(..))
import Data.Text                (Text, pack)
import Data.HashMap             (Map, empty, insert, lookup)
import Data.Function            (id)
import Control.Monad.Except     (MonadError, runExceptT, catchError, throwError)
import Control.Exception        (IOException(..), SomeException(..), Exception, throw)
import Data.Typeable            (Typeable)
import Text.Read                (readMaybe)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Foldable            (msum)

type Key = Text
type Val = Text

data Err = Err deriving (Eq, Show)

--data TestF n where
--  Log      :: (MonadError Err m) => Text -> (m () -> n)         -> TestF n
--  Get      :: (MonadError Err m) => Key -> (m Val -> n)         -> TestF n
--  Put      :: (MonadError Err m) => Key -> Val -> (m () -> n)   -> TestF n
--  Rollback :: (MonadError Err m) => (m () -> n)                 -> TestF n
--  Commit   :: (MonadError Err m) => (m () -> n)                 -> TestF n
--  Transact :: (MonadError Err m) => TestFree m a -> (m a -> n)  -> TestF n

data TestF n where
  Log      :: (MonadError Err m) => Text -> (m () -> n)         -> TestF n
  Get      :: (MonadError Err m) => Key -> (m Val -> n)         -> TestF n
  Put      :: (MonadError Err m) => Key -> Val -> (m () -> n)   -> TestF n
  Rollback :: (MonadError Err m) => (m () -> n)                 -> TestF n
  Commit   :: (MonadError Err m) => (m () -> n)                 -> TestF n
--  Transact :: (MonadError Err m) => TestFree m a -> (m a -> n)  -> TestF n

instance Functor TestF where
  fmap f (Log msg n) = Log msg (f . n)
  fmap f (Get k n) = Get k (f . n)
  fmap f (Put k v n) = Put k v (f . n)
  fmap f (Rollback n) = Rollback (f . n)
  fmap f (Commit n) = Commit (f . n)
--  fmap f (Transact block n) = Transact block (f . n)

type TestFree m a = FreeT TestF m a
--  deriving (Applicative, Functor, Monad, MonadFree (TestF tex))

makeFree ''TestF

--log      :: (MonadFree TestF m) => Text -> m ()
--get      :: (MonadFree TestF m) => Key -> m Val
--put      :: (MonadFree TestF m) => Key -> Val -> m ()
--transact :: (MonadFree TestF m) => TestFree m a -> m a
--rollback :: (MonadFree TestF m) => m ()
--commit   :: (MonadFree TestF m) => m ()

-- either :: (a -> c) -> (b -> c) -> Either a b -> c
-- runExceptT :: ExceptT e m a -> m (Either e a)
err :: (MonadError Err m) => Either Err a -> m a
err = either throwError return

a :: (MonadFree TestF m, MonadError Err m) => Text -> m ()
a = undefined

someFn :: ExceptT Err IO ()
someFn = err (Left Err)

main :: IO ()
main = do
  eth <- runExceptT someFn
  either print return eth

data RetryF next where
  Output    :: String -> (Either Err () -> next) -> RetryF next
  Input     :: Read a => (Either Err a -> next)  -> RetryF next
  Transact  :: RetryFree a -> (a -> next) ->        RetryF next
  Retry     ::                                      RetryF next

instance Functor RetryF where
  fmap f (Output s x) = Output s (f . x)
  fmap f (Input g) = Input (f . g)
  fmap f (Transact block g) = Transact block (f . g)
  fmap _ Retry = Retry

type RetryFree = Free RetryF

makeFree ''RetryF

output1 :: MonadFree RetryF m => String -> m (Either Err ())
output1 s = liftF $ Output s id

input1 :: (MonadFree RetryF m, Read a) => m (Either Err a)
input1 = liftF $ Input id

transact1 :: MonadFree RetryF m => RetryFree a -> m a
transact1 block = liftF $ Transact block id

retry1 :: MonadFree RetryF m => m ()
retry1 = liftF Retry

--abc2 :: (MonadError Err m0, MonadFree RetryF m1) => m1 (m0 a)
--abc2 :: RetryFree (Either Err a)
--abc2 = input2

type VM a = RetryFree (Either Err a)

test :: VM ()
test = runExceptT $ do
  n <- ExceptT $ transact1 $ do
    n <- input1
--    when (n <= 0) $ do
--      lift $ output1 "The number should be positive."
--      retry1
    return $ Right n
  ExceptT $ output1 $ "You've just entered " ++ show (n :: Either Err Int)
--  n <- lift $ withRetry1 $ do
--    lift $ output1 "Enter any positive number: "
--    n <- lift input1
--    when (n <= 0) $ do
--      lift $ output1 "The number should be positive."
--      retry1
--    return n
--  lift $ output1 $ "You've just entered " ++ show (n :: Int)

runRetry :: MonadIO m => RetryFree a -> m a
runRetry = iterM runIO

runIO :: MonadIO m => RetryF (m a) -> m a
runIO (Output s next) = do
  liftIO $ putStrLn s
  next $ Right ()
runIO (Input next) = do
  s <- liftIO getLine
  let eth = readE s
  next eth -- eth :: Either Err a
  where
    readE :: (Read a1) => String -> Either Err a1
    readE s = case readMaybe s of
      Just x -> Right x
      Nothing -> Left Err
runIO (Transact block next) = do
  -- Here we use
  -- runRetry :: MonadIO m => Retry a -> MaybeT (m a)
  -- to control failure with MaybeT.
  -- We repeatedly run retriable block until we get it to work.
  Just x <- runMaybeT . msum $ repeat (runRetry block)
  next x
runIO Retry = fail "forced retry"


--foo :: (MonadFree TestF m, MonadError Err m) => m ()
--foo = do
--  x <- get "k1"
--  put "k2" x
--  throwError Err
--  z <- bar
--  log $ pack $ show z
--
--bar :: (MonadFree TestF m) => m Int
--bar = do
--  y <- get "k3"
--  log $ pack $ show y
--  when (y == "42")
--    rollback
--  put "k4" y
--  return (42 :: Int)

-- > {-# OPTIONS_GHC -Wall                      #-}
-- > {-# OPTIONS_GHC -fno-warn-name-shadowing   #-}
-- > {-# OPTIONS_GHC -fno-warn-type-defaults    #-}
-- > {-# OPTIONS_GHC -fno-warn-unused-do-bind   #-}
-- > {-# OPTIONS_GHC -fno-warn-missing-methods  #-}
-- > {-# OPTIONS_GHC -fno-warn-orphans          #-}

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
