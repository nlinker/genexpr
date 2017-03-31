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

type Key = Text
type Val = Text

data Err = Err

data TestF n where
  Log      :: (MonadError Err m) => Text -> (m () -> n)         -> TestF n
  Get      :: (MonadError Err m) => Key -> (m Val -> n)         -> TestF n
  Put      :: (MonadError Err m) => Key -> Val -> (m () -> n)   -> TestF n
  Rollback :: (MonadError Err m) => (m () -> n)                 -> TestF n
  Commit   :: (MonadError Err m) => (m () -> n)                 -> TestF n
  Transact :: (MonadError Err m) => TestFree m a -> (m a -> n)  -> TestF n

instance Functor TestF where
  fmap f (Log msg n) = Log msg (f . n)
  fmap f (Get k n) = Get k (f . n)
  fmap f (Put k v n) = Put k v (f . n)
  fmap f (Transact block n) = Transact block (f . n)
  fmap f (Rollback n) = Rollback (f . n)
  fmap f (Commit n) = Commit (f . n)

type TestFree m a = FreeT TestF m a
--  deriving (Applicative, Functor, Monad, MonadFree (TestF tex))

--makeFree ''TestF

log      :: (MonadFree TestF m) => Text -> m ()
get      :: (MonadFree TestF m) => Key -> m Val
put      :: (MonadFree TestF m) => Key -> Val -> m ()
transact :: (MonadFree TestF m) => TestFree m a -> m a
rollback :: (MonadFree TestF m) => m ()
commit   :: (MonadFree TestF m) => m ()

--ret :: ExceptT e m a -> m (Either e a)
ret :: ExceptT Err m a -> m (Either Err a)
ret = runExceptT

--y :: MonadError Err m => m ()
y :: ExceptT Err m ()
y = ExceptT (_ (Right ()))



rollback = return undefined
commit = undefined
log = undefined
get = undefined
put = undefined
transact = undefined

data Picture tex = Picture tex

data Cmd tex n =
    Clear n
  | LoadTexture FilePath (tex -> n)
  | Render (Picture tex) n
  | Update n
    deriving Functor

newtype Command tex m a = Command { runCommand :: FreeT (Cmd tex) m a }
  deriving (Applicative, Functor, Monad, MonadFree (Cmd tex), MonadTrans)

-- > {-# OPTIONS_GHC -Wall                      #-}
-- > {-# OPTIONS_GHC -fno-warn-name-shadowing   #-}
-- > {-# OPTIONS_GHC -fno-warn-type-defaults    #-}
-- > {-# OPTIONS_GHC -fno-warn-unused-do-bind   #-}
-- > {-# OPTIONS_GHC -fno-warn-missing-methods  #-}
-- > {-# OPTIONS_GHC -fno-warn-orphans          #-}


foo :: (MonadFree TestF m, MonadError Err m) => m ()
foo = do
  x <- get "k1"
  put "k2" x
  throwError Err
  z <- transact $ do
    y <- get "k3"
    log $ pack $ show y
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
