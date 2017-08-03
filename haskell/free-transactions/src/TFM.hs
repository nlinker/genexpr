--{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module TFM where

import           Debug.Trace

import           Control.Exception         (Exception, IOException (..), SomeException (..), throw)
import           Control.Monad             (when)
import           Control.Monad.Except      (ExceptT (..), MonadTrans, lift)
import           Control.Monad.Except      (MonadError, catchError, runExceptT, throwError)
import           Control.Monad.Free
import           Control.Monad.Free.TH     (makeFree, makeFreeCon)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Trans.Free  (FreeT (..))
import           Control.Monad.Trans.Maybe (runMaybeT)
import           Data.Foldable             (msum)
import           Data.Function             (id)
import           Data.HashMap              (Map, empty, insert, lookup)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text, pack)
import           Data.Typeable             (Typeable)
import           Prelude                   hiding (log)
import           Text.Read                 (readMaybe)

data Err = Err Text deriving (Eq, Show)

type TFM a = FreeT TransF (Either Err)

-- FreeT (f :: * -> *) (m :: * -> *) a
type TransFree = Free TransF

data TransF next where
  Output    :: String ->                  (Either Err () -> next) -> TransF next
  Input     :: Read a =>                  (Either Err a -> next)  -> TransF next
  Transact  :: Show a => TransFree (Either Err a) -> (Either Err a -> next)  -> TransF next
  Rollback  ::                            (Either Err () -> next) -> TransF next
  Commit    ::                            (Either Err () -> next) -> TransF next

instance Functor TransF where
  fmap f (Output s n) = Output s (f . n)
  fmap f (Input n) = Input (f . n)
  fmap f (Transact block n) = Transact block (f . n)
  fmap f (Rollback n) = Rollback (f . n)
  fmap f (Commit n) = Commit (f . n)

makeFree ''TransF

-- runExceptT :: ExceptT e m a -> m (Either e a)

output1 :: String -> ExceptT Err TransFree ()
output1 s = ExceptT $ liftF $ Output s id

input1 :: (Read a) => ExceptT Err TransFree a
input1 = ExceptT $ liftF $ Input id

--transact :: Show a => TransFree (Either Err a) -> TransFree (Either Err a)

transact1 :: Show a => ExceptT Err TransFree a -> ExceptT Err TransFree a
transact1 = ExceptT . inner . runExceptT
  where
    inner :: Show a => TransFree (Either Err a) -> TransFree (Either Err a)
    inner block = liftF $ Transact block id

rollback1 :: ExceptT Err TransFree ()
rollback1 = ExceptT $ liftF $ Rollback id

commit1 :: ExceptT Err TransFree ()
commit1 = ExceptT $ liftF $ Commit id

main :: IO ()
main = do
  putStrLn "=== Explicit branching, fireIO testN ==="
  n <- fireIO testN
  putStrLn $ "fireIO testN = " <> show n
  putStrLn
  putStrLn "=== Implicit branching, fireIO testY ==="
  y <- fireIO testY
  putStrLn $ "fireIO testY = " <> show y

testN :: TransFree (Either Err Integer)
testN = do
  (ke :: Either Err Integer) <- input
  case ke of
    Left e -> return $ Left e
    Right k -> do
      output $ "k = " <> show k
      ne <- transact $ do
        (ne' :: Either Err Integer) <- input
        case ne' of
          Left e -> return $ Left e
          Right n' -> do
            ie <- if n' >= 0 then do
              output "The number should be negative."
              rollback
            else
              return $ Right ()
            case ie of
              Left e -> return $ Left e
              Right () -> return $ Right n'
      case ne of
        Left e -> return $ Left e
        Right n -> do
          output $ "You've just entered " ++ show n
          return $ Right n

testY :: TransFree (Either Err Integer)
testY = runExceptT $ do
  (k :: Integer) <- input1
  output1 $ "k = " <> show k
  (n :: Integer) <- transact1 $ do
    n' <- input1
    when (n' >= 0) $ do
      output1 "The number should be negative."
      rollback1
    return n'
  output1 $ "You've just entered " ++ show n
  return n

fireIO :: MonadIO m => TransFree a -> m a
fireIO = iterM runIO

runIO :: MonadIO m => TransF (m a) -> m a
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
      Nothing -> Left $ Err $ "cannot read " <> pack s
runIO (Transact block next) = do
  -- eth :: Either Err a
  -- fireIO block :: m (Either Err a)
  -- fireIO :: MonadIO m => TransFree a -> m a
  -- block :: TransFree (Either Err a)
  -- runExceptT :: ExceptT e m a -> m (Either e a)
  eth <- fireIO block
  traceM $ "----> " <> show eth
  case eth of
    Left err -> next $ Left err
    Right a -> next $ Right a
runIO (Rollback next) = next $ Left $ Err "reset"
runIO (Commit next) = next $ Right $ error "TODO commit is not implemented yet"


-- TODO how to make free monads with monad constraints
--type TestFree m a = FreeT TestF m a

--data TestF n where
--  Log      :: (MonadError Err m) => Text -> (m () -> n)         -> TestF n
--  Get      :: (MonadError Err m) => Key -> (m Val -> n)         -> TestF n
--  Put      :: (MonadError Err m) => Key -> Val -> (m () -> n)   -> TestF n
--  Rollback :: (MonadError Err m) => (m () -> n)                 -> TestF n
--  Commit   :: (MonadError Err m) => (m () -> n)                 -> TestF n
--  Transact :: (MonadError Err m) => TestFree m a -> (m a -> n)  -> TestF n

--instance Functor TestF where
--  fmap f (Log msg n) = Log msg (f . n)
--  fmap f (Get k n) = Get k (f . n)
--  fmap f (Put k v n) = Put k v (f . n)
--  fmap f (Rollback n) = Rollback (f . n)
--  fmap f (Commit n) = Commit (f . n)
--  fmap f (Transact block n) = Transact block (f . n)
--makeFree ''TestF

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
