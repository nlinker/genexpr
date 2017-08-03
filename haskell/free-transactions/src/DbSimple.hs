{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DbSimple where

import Prelude hiding (id, lookup)
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
import Data.Function          ((&))

import Data.Pool (Pool, createPool, withResource)
import Database.PostgreSQL.Simple (Query, ConnectInfo(..), Connection, Only,
  query, connect, close, withTransaction, execute)
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Database.PostgreSQL.Simple.ToRow (ToRow, toRow)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Model

-- run foo in IO
mainSimple :: IO ()
mainSimple = do
  pool <- createPool (connect defaultSettings) close 10 0.5 10
  stuffs <- withTx pool $ \conn -> do
    x1 <- stuffInsert conn (Stuff 0 "stuff1" 12)
    print x1
    x2 <- stuffInsert conn (Stuff 0 "stuff2" 23)
    print x2
    x3 <- stuffInsert conn (Stuff 0 "stuff3" 34)
    print x3
    (ss :: [Stuff]) <- stuffSelectAll conn
    return ss
  print stuffs

instance FromRow Stuff where
  fromRow = Stuff <$> field <*> field <*> field
instance ToRow Stuff where
  toRow p = toRow (name p, age p)

stuffSelectAll :: Connection -> IO [Stuff]
stuffSelectAll c = query c sqlText () where
  sqlText = [sql| SELECT id, name, age FROM stuff; |]

stuffInsert :: Connection -> Stuff -> IO [Only Int]
stuffInsert c s = query c sqlText s where
  sqlText = [sql| INSERT INTO stuff (name, age) VALUES (?, ?) RETURNING id; |]

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
  run (GetVal k next) = next =<< mv where
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
