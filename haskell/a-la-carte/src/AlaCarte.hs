{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE OverloadedStrings          #-}
-- {-# LANGUAGE UndecidableInstances       #-}

module AlaCarte where

import           Control.Monad.Free
import           Control.Monad.Free.TH        
import           Data.Monoid                  ((<>))
import           Control.Monad.IO.Class       (MonadIO, liftIO)
-- Combine multiple functors (dsl-s) together
-- https://ocharles.org.uk/blog/posts/2016-01-26-transformers-free-monads-mtl-laws.html
-- http://stackoverflow.com/questions/21395407/combining-free-types
-- https://www.reddit.com/r/haskell/comments/5cbopj/combining_functors_on_free_monads/

-- These are our operations, similar like DbDsl, SnsDsl, UserServiceDsl, etc.
-- first dsl
data Dsl1F n =
    Create Int n
  | GetCreate (Int -> n) deriving (Functor)
runDsl1IO :: (MonadIO m) => Dsl1F (m a) -> m a
runDsl1IO (Create i n) = liftIO (putStrLn $ "create(" <> show i <> ")") >> n
runDsl1IO (GetCreate r) = liftIO (putStrLn "getCreate -> 5") >> r 5

-- second dsl
data Dsl2F n =
    Run String n
  | GetRun (String -> n) deriving (Functor)
runDsl2IO :: (MonadIO m) => Dsl2F (m a) -> m a
runDsl2IO (Run s n) = liftIO (putStrLn $ "run(" <> show s <> ")") >> n
runDsl2IO (GetRun r) = liftIO (putStrLn "getRun -> yo!") >> r "yo!"

-- third dsl
data Dsl3F n =
    Finish Float n
  | GetFinish (Float -> n) deriving (Functor)
runDsl3IO :: (MonadIO m) => Dsl3F (m a) -> m a
runDsl3IO (Finish b n) = liftIO (putStrLn $ "finish(" <> show b <> ")") >> n
runDsl3IO (GetFinish r) = liftIO (putStrLn "getFinish -> 7.9") >> r 7.9

-- forth dsl
data Dsl4F n =
    Go [Int] n
  | GetGo ([Int] -> n) deriving (Functor)
runDsl4IO :: (MonadIO m) => Dsl4F (m a) -> m a
runDsl4IO (Go bs n) = liftIO (putStrLn $ "go(" <> show bs <> ")") >> n
runDsl4IO (GetGo r) = liftIO (putStrLn "getGo -> []") >> r [1, 2, 3]

-- union for all of the dsl-s
data SumF n =
    S1 (Dsl1F n)
  | S2 (Dsl2F n)
  | S3 (Dsl3F n)
  | S4 (Dsl4F n)
  deriving (Functor)

type FreeSum = Free SumF

runSumIO :: (MonadIO m) => SumF (m a) -> m a
runSumIO (S1 dsl) = runDsl1IO dsl
runSumIO (S2 dsl) = runDsl2IO dsl
runSumIO (S3 dsl) = runDsl3IO dsl
runSumIO (S4 dsl) = runDsl4IO dsl

makeFree ''Dsl1F
makeFree ''Dsl2F
makeFree ''Dsl3F
makeFree ''Dsl4F

-- S1 :: Dsl1F n -> SumF n
-- hoistFree :: Functor g => (forall a. f a -> g a) -> Free f b -> Free g b
-- Free :: f (Free f a) -> Free f a
-- create :: MonadFree Dsl1F m0 => Int -> m0 ()
-- create :: Int -> (Free Dsl1F) ()

-- sCreate :: Int -> FreeSum ()
-- sCreate i = liftF $ S1 $ Create i ()

-- allCreate :: MonadFree SumF m => Int -> m ()
allCreate :: Int -> Free SumF ()
allCreate = hoistFree S1 . create

allGetCreate :: Free SumF Int
allGetCreate = hoistFree S1 getCreate

allRun :: String -> Free SumF ()
allRun = hoistFree S2 . run

allGetRun :: Free SumF String
allGetRun = hoistFree S2 getRun

allFinish :: Float -> Free SumF ()
allFinish = hoistFree S3 . finish

allGetFinish :: Free SumF Float
allGetFinish = hoistFree S3 getFinish

allGo :: [Int] -> Free SumF ()
allGo = hoistFree S4 . go

allGetGo :: Free SumF [Int]
allGetGo = hoistFree S4 getGo

---- handmade operations against SumF are like this
-- sCreate :: Int -> Free Sum m
-- sCreate i = liftF $ S1 $ Create i ()

logic :: Free SumF [Int]
logic = do
  allCreate 42
  x <- allGetCreate
  allRun $ show x
  y <- allGetRun
  allFinish $ fromIntegral (length y)
  z <- allGetFinish
  allGo $ replicate (fromInteger . floor $ z * 10.0) 5
  allGetGo

mainA :: IO ()
mainA = do
  x <- iterM runSumIO logic
  putStrLn $ "logic returned " <> show x


-- import           Control.Monad.Except         (MonadError)
-- import           Control.Monad.Catch          (MonadThrow)
-- import           Control.Monad.IO.Class       (MonadIO)
-- import           Control.Monad.Reader         (MonadReader)
-- import           Control.Monad.Trans.Control  (MonadBaseControl)
-- import           Control.Monad.State          (MonadState)
-- import           Data.Text                    (Text)
-- import           Vertigo.Types.Error          (ErrorEnum (..))
-- import           Vertigo.Ext                  (Ext)
-- import           Vertigo.Convertible          ()
-- import           Vertigo.Types.DeviceInfo     (DeviceInfo, DeviceToken(..), EndpointArn(..))
--
-- import qualified Data.Map                            as M
-- import qualified System.Logging.Facade               as Log
-- import qualified Vertigo.Types.Notification.Ids      as N
-- import qualified Vertigo.Types.Notification.UserCore as N
-- import           Control.Monad.Free
-- import           Control.Monad.Free.TH        (makeFree)
-- import           Control.Monad.Free.TH        (makeFree, makeFreeCon)
-- import           Control.Lens                 ((&), (?~), (^.))
-- import           Data.Monoid                  ((<>))
-- import           Data.Text                    (Text)
-- import           Control.Monad.Reader         (MonadReader, ask, asks)
-- import           Control.Monad.IO.Class       (MonadIO, liftIO)
--                                              
-- import           Helper.Convert               (convToVuid)
-- import           Vertigo.Dsl.Environment      (ProductionEnv)
-- import           Vertigo.Ext                  (Ext, poolSettings)
--
-- import qualified Vertigo.Types.Notification          as N
-- import qualified Vertigo.Types.Notification.Ids      as N
-- import qualified Vertigo.Types.Notification.UserCore as N

