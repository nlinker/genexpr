{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module DbHasql where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid            ((<>))
import Data.Functor.Contravariant (contramap)
import Data.Int               (Int32)
import Data.Text              (Text)

import qualified Hasql.Transaction as HT
import qualified Hasql.Transaction.Sessions as HTS
import qualified Hasql.Connection as HC
import qualified Hasql.Session as HS
import qualified Hasql.Query as HQ
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD

import Model

-- Try transactions with Hasql library
mainHasql :: IO ()
mainHasql = do
  connE <- HC.acquire defaultSettings
  runE <- case connE of
    Left err -> error $ show err
    Right conn -> HS.run exampleTransaction2 conn
  print runE

defaultSettings :: HC.Settings
defaultSettings = HC.settings dbHost dbPort dbUser dbPassword dbDatabase
  where
    dbHost = "localhost"
    dbPort = 5432
    dbUser = "postgres"
    dbPassword = "postgres"
    dbDatabase = "notification_db"

-- TODO WARNING: it seems impossible to integrate it with free monad
myTrans2 :: HT.Transaction [Stuff]
myTrans2 = do
  _id1 <- HT.query (Stuff 0 "stuff1" 12) insertStuff
--  liftIO $ print id1
  _id2 <- HT.query (Stuff 0 "stuff2" 23) insertStuff
--  liftIO $ print id2
  _id3 <- HT.query (Stuff 0 "stuff3" 34) insertStuff
--  liftIO $ print id3
  HT.query () readAllStuff

exampleTransaction2 :: HS.Session [Stuff]
exampleTransaction2 = do
  liftIO $ putStrLn "Hey"
  HTS.transaction HT.ReadCommitted HT.Write myTrans2

insertStuff :: HQ.Query Stuff Int32
insertStuff = stmt
  where
    stmt = HQ.statement sqlText encoder decoder True
    sqlText = "INSERT INTO stuff (name, age) VALUES ($1, $2) RETURNING id;"
    encoder =
      contramap name (HE.value HE.text) <>
      contramap (fromIntegral . age) (HE.value HE.int4)
    decoder =
      HD.singleRow (HD.value HD.int4)

readAllStuff :: HQ.Query () [Stuff]
readAllStuff = stmt
  where
    stmt = HQ.statement sqlText encoder decoder True
    sqlText = "SELECT id, name, age FROM stuff;"
    encoder =
      HE.unit
    decoder =
      HD.rowsList $ Stuff
        <$> (fromIntegral <$> HD.value HD.int4)
        <*> HD.value HD.text
        <*> (fromIntegral <$> HD.value HD.int4)
