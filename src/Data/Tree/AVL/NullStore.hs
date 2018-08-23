module Data.Tree.AVL.NullStore where

import           Control.Monad.Catch
import           Control.Monad.State

import           Data.ByteString        (ByteString)
import           Data.Hashable
import           Data.HashMap.Strict    as HM
import           Data.Typeable

import           Data.Tree.AVL.Internal

type NullStore = IO

newtype NullStoreT m a = NullStoreT { runNullStoreT :: m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

instance MonadTrans NullStoreT where
    lift = NullStoreT

instance (Show k, Typeable k, Serialisable k) => KVStoreMonad k NullStore where
    retrieve k = throwM (NotFound k)
    store _ _  = return ()

instance (Show k, Typeable k, Serialisable k, MonadCatch m, MonadIO m) => KVStoreMonad k (NullStoreT m) where
    retrieve k = throwM (NotFound k)
    store _ _  = return ()

sandboxedT :: (Show k, Eq k, Typeable k, Serialisable k, MonadCatch m, MonadIO m) => HashMapStore k (NullStoreT m) a -> HashMapStore k m a
sandboxedT action = do
    (res, _) <- lift $ runNullStoreT $ runOnEmptyCache action
    return res

sandboxed :: (Show k, Eq k, Typeable k, Serialisable k, MonadIO m) => HashMapStore k NullStore a -> HashMapStore k m a
sandboxed action = do
    (res, _) <- liftIO $ runOnEmptyCache action
    return res

dumpDatabase :: (Show k, MonadIO m) => HashMapStore k m ()
dumpDatabase = do
    st <- get
    liftIO $ putStrLn (show st)
