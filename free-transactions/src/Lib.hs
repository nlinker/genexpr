{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Lib where

import Prelude hiding (lookup)
import Control.Monad.Free
import Control.Monad.Trans.Free (FreeT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Free.TH  (makeFree, makeFreeCon)
import Data.IORef             (IORef, newIORef, modifyIORef, readIORef)
import System.IO.Unsafe       (unsafePerformIO)
import Data.ByteString.Lazy   (ByteString)
import Data.Text              (Text, pack, unpack)
import Data.HashMap           (Map, empty, insert, lookup)
import Data.Monoid            ((<>))
import Data.Maybe             (fromMaybe)

type Key = Text
type Val = Text

data OpsF next where
  -- default Val will be "" just not to fiddle with options
  Get :: Key -> (Val -> next) -> OpsF next
  Put :: Key -> Val -> next -> OpsF next
  Transact :: Ops a -> (a -> next) -> OpsF next

instance Functor OpsF where
  fmap f (Get k c) = Get k (f . c)
  fmap f (Put k v n) = Put k v (f n)
  fmap f (Transact block c) = Transact block (f . c)

type Ops = Free OpsF

-- magic
makeFree ''OpsF

-- run foo in IO
runFoo :: IO ()
runFoo = do
  m <- runOpsIO foo
  print m

foo :: Ops (Map Key Val)
foo = do
  let k1 = "foo1"
  let k2 = "foo2"
  put k1 "value1"
  put k2 "value2"
  transact $ do
    (vt1, vt2) <- transact $ do
      v1 <- get k1
      v2 <- get k2
      return (v1, v2)
    transact $ do
      put k1 vt2
      put k2 vt1
  -- get one by one, not in transaction
  vn1 <- get k1
  vn2 <- get k2
  return $
    insert k1 vn1 $
    insert k2 vn2 empty

-- now run it in the IO
runOpsIO :: MonadIO m => Ops a -> m a
runOpsIO = iterM run where
  run :: MonadIO m => OpsF (m a) -> m a
  run (Get k c) = c =<< mv where
    mv = liftIO $ do
      v <- globalGet k
      indent <- globalIndent
      putStrLn $ unpack $ indent <> "get " <> k <> " -> " <> v
      return v
  run (Put k v n) = const n =<< mv where
    mv = liftIO $ do
      globalPut k v
      indent <- globalIndent
      putStrLn $ unpack $ indent <> "put " <> k <> " " <> v
  run (Transact block c) = c =<< mv where
    mv = liftIO $ do
      indent <- globalIndent
      putStrLn $ unpack $ indent <> "start transaction"
      globalModifyIndent (+ 1)
      r <- runOpsIO block
      globalModifyIndent (\x -> x - 1)
      putStrLn $ unpack $ indent <> "end transaction"
      return r

-- global hash map
{-# NOINLINE _globalKV #-}
_globalKV :: IORef (Map Key Val)
_globalKV = unsafePerformIO $ newIORef empty

{-# NOINLINE _globalI #-}
_globalI :: IORef Int
_globalI = unsafePerformIO $ newIORef 0

globalGet :: Key -> IO Val
globalGet k = do
  m <- readIORef _globalKV
  return $ fromMaybe "" $ lookup k m

globalPut :: Key -> Val -> IO ()
globalPut k v = modifyIORef _globalKV $ \m -> insert k v m

globalIndent :: IO Text
globalIndent = do
  i <- readIORef _globalI
  return $ pack $ replicate (i * 2) ' '

globalModifyIndent :: (Int -> Int) -> IO ()
globalModifyIndent = modifyIORef _globalI
