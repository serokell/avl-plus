-- | A stub storage that throws 'NotFound' on any 'retrieve'.

module Data.Tree.AVL.Store.Void
    ( -- * No-storage transformer.
      StoreT (..)
      -- * No-storage monad.
    , Store
    ) where

import Control.Monad.Catch
import Control.Monad.State
import Data.Typeable (Typeable)

import Data.Tree.AVL.Internal

-- | Wrapper. Cancels any capabilities of @m@ to store stuff.
newtype StoreT m a = StoreT { runStoreT :: m a }
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance MonadTrans StoreT where
    lift = StoreT

-- | This "storage" always throws when you try to read from it.
--   It does this, because on light client you haven't any storage,
--   (all you have is a tree from proof).
--
--   So, if operations to be proven require you to read from storage,
--   its an error (namely, 'NotFound').
instance (Show k, Typeable k, MonadCatch m, MonadIO m) => KVRetrieve k n (StoreT m) where
    retrieve k = throwM (NotFound k)

-- | Storage monad that fails on any access attempt.
type Store = StoreT IO
