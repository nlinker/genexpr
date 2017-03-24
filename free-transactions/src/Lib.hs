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
import Control.Monad          (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Free (FreeT)
import Control.Monad.Free.TH  (makeFree, makeFreeCon)
import Data.IORef             (IORef, newIORef, modifyIORef, readIORef)
import System.IO.Unsafe       (unsafePerformIO)
import Data.Functor.Contravariant (contramap)
import Data.Int               (Int32, Int64)
import Data.HashMap           (Map, empty, insert, lookup)
import Data.Monoid            ((<>))
import Data.Maybe             (fromMaybe)
import Data.Text              (Text, pack, unpack)


import qualified Hasql.Transaction as HT
import qualified Hasql.Transaction.Sessions as HTS
import qualified Hasql.Connection as HC
import qualified Hasql.Session as HS
import qualified Hasql.Query as HQ
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

data Stuff = Stuff
  { id :: Int
  , name :: Text
  , age :: Int
  } deriving (Show, Eq)

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

defaultSettings :: HC.Settings
defaultSettings = HC.settings dbHost dbPort dbUser dbPassword dbDatabase
  where
    dbHost = "localhost"
    dbPort = 5432
    dbUser = "postgres"
    dbPassword = "postgres"
    dbDatabase = "notification_db"

insertStuff :: HQ.Query Stuff Int32
insertStuff = stmt
  where
    stmt = HQ.statement sqlText encoder decoder True
    sqlText = "INSERT INTO stuffs (name, age) VALUES ($1, $2) RETURNING id;"
    encoder =
      contramap name (HE.value HE.text) <>
      contramap (fromIntegral . age) (HE.value HE.int4)
    decoder =
      HD.singleRow (HD.value HD.int4)

readAllStuff :: HQ.Query () [Stuff]
readAllStuff = stmt
  where
    stmt = HQ.statement sqlText encoder decoder True
    sqlText = "SELECT id, name, age FROM stuffs;"
    encoder =
      HE.unit
    decoder =
      HD.rowsList $ Stuff
        <$> (fromIntegral <$> HD.value HD.int4)
        <*> HD.value HD.text
        <*> (fromIntegral <$> HD.value HD.int4)

-- TODO WARNING: it seems impossible to integrate it with Free OpsF
trans2 :: HT.Transaction [Stuff]
trans2 = do
  id1 <- HT.query (Stuff 0 "stuff1" 12) insertStuff
--  liftIO $ print id1
  id2 <- HT.query (Stuff 0 "stuff2" 23) insertStuff
--  liftIO $ print id2
  id3 <- HT.query (Stuff 0 "stuff3" 34) insertStuff
--  liftIO $ print id3
  HT.query () readAllStuff

exampleTransaction2 :: HS.Session [Stuff]
exampleTransaction2 = do
  liftIO $ putStrLn "Hey"
  HTS.transaction HT.ReadCommitted HT.Write trans2

exampleTransaction :: HS.Session ()
exampleTransaction = HTS.transaction HT.ReadCommitted HT.Write trans
  where
    prepareSqlExec :: BS.ByteString -> HQ.Query () ()
    prepareSqlExec sql = HQ.statement sql HE.unit HD.unit False

    queries :: [HQ.Query () ()]
    queries = prepareSqlExec <$> ["SELECT 1", "SELECT 2"]

    trans :: HT.Transaction ()
    trans = void (sequence $ HT.query () <$> queries)

-- do this here
kick :: IO ()
kick = do
  connE <- HC.acquire defaultSettings
  runE <- case connE of
    Left err -> error $ show err
    Right conn -> HS.run exampleTransaction2 conn
  print runE

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
