-- | A stub storage that throws 'NotFound' on any 'retrieve'.

module Data.Tree.AVL.Store.Void
    ( -- * No-storage transformer.
      StoreT (..)
      -- * No-storage monad.
    , Store
    ) where

import Control.Monad.Catch
import Control.Monad.State

import Data.Typeable

import Data.Tree.AVL.Internal

-- | Wrapper. Cancels any capabilities of @m@ to store stuff.
newtype StoreT m a = StoreT { runStoreT :: m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

instance MonadTrans StoreT where
    lift = StoreT

instance (Show k, Typeable k, MonadCatch m, MonadIO m) => KVRetrieve k n (StoreT m) where
    retrieve k = throwM (NotFound k)

-- | IO Wrapper that fails on any access attempt.
type Store = StoreT IO
