{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module DbSimple where

import Prelude hiding (lookup)
import Control.Monad.Free     (iterM)
import Control.Monad.Random   (MonadRandom)
import Control.Monad          (void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef             (IORef, newIORef, modifyIORef, readIORef)
import System.IO.Unsafe       (unsafePerformIO)
import Data.Int               (Int32, Int64)
import Data.HashMap           (Map, empty, insert, lookup)
import Data.Monoid            ((<>))
import Data.Maybe             (fromMaybe)
import Data.Text              (Text, pack, unpack)

import Data.Pool (Pool, createPool, withResource)
import Database.PostgreSQL.Simple (Query, ConnectInfo(..), Connection,
  Only, query, connect, close, withTransaction, execute)
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Database.PostgreSQL.Simple.ToRow (ToRow, toRow)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Model

instance FromRow Stuff where
  fromRow = Stuff <$> field <*> field <*> field
instance ToRow Stuff where
  toRow p = toRow (name p, age p)

-- run foo in IO
mainSimple :: IO ()
mainSimple = do
  pool <- createPool (connect defaultSettings) close 10 0.5 10
  stuffs <- withTx pool $ \conn -> do
    execute conn stuffInsert (Stuff 0 "stuff1" 12)
    execute conn stuffInsert (Stuff 0 "stuff2" 23)
    execute conn stuffInsert (Stuff 0 "stuff3" 34)
    return $ execute conn stuffSelectAll
  print stuffs

withTx :: Pool Connection -> (Connection -> IO a) -> IO a
withTx pool ioa = withResource pool (\conn ->
  withTransaction conn (ioa conn))

defaultSettings :: ConnectInfo
defaultSettings = ConnectInfo dbHost dbPort dbUser dbPassword dbDatabase
  where
    dbHost = "localhost"
    dbPort = 5432
    dbUser = "postgres"
    dbPassword = "postgres"
    dbDatabase = "notification_db"

stuffSelectAll :: Query
stuffSelectAll = [sql| SELECT id, name, age FROM stuff; |]

stuffInsert :: Query
stuffInsert = [sql| INSERT INTO stuff (name, age) VALUES (?, ?) RETURNING id; |]


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
    insert k1 vn1 $
    insert k2 vn2 empty

-- now run it in the IO
runOpsIO :: (MonadIO m, MonadRandom m) => Ops a -> m a
runOpsIO = iterM run where
  run :: MonadIO m => OpsF (m a) -> m a
  run (GetX k next) = next =<< mv where
    mv = liftIO $ do
      v <- globalGet k
      indent <- globalIndent
      putStrLn $ unpack $ indent <> "get " <> k <> " -> " <> v
      return v
  run (GetDb k next) = next =<< mv where
    mv = liftIO $ do
      v <- globalGet k
      indent <- globalIndent
      putStrLn $ unpack $ indent <> "get " <> k <> " -> " <> v
      return v
  run (Failure next) = next
  run (Transact block next) = next =<< mv where
    mv = liftIO $ do
      indent <- globalIndent
      putStrLn $ unpack $ indent <> "start transaction"
      globalModifyIndent (+ 1)
      r <- runOpsIO block
      globalModifyIndent (\x -> x - 1)
      putStrLn $ unpack $ indent <> "end transaction"
      return r

-- global connection
{-# NOINLINE _globalConnection #-}
_globalConnection :: IORef Connection
_globalConnection = undefined

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
