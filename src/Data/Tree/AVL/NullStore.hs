module Data.Tree.AVL.NullStore where

import           Control.Monad.Catch
import           Control.Monad.State

import           Data.Typeable

import           Data.Tree.AVL.Internal

type NullStore = IO

newtype NullStoreT m a = NullStoreT { runNullStoreT :: m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

instance MonadTrans NullStoreT where
    lift = NullStoreT

instance (Show k, Typeable k) => KVRetrieve k n NullStore where
    retrieve k = throwM (NotFound k)

instance (Show k, Typeable k, MonadCatch m, MonadIO m) => KVRetrieve k n (NullStoreT m) where
    retrieve k = throwM (NotFound k)
