-- | The worst storage possible.

module Data.Tree.AVL.Store.Void
    ( -- * No-storage monad
      VoidStorage

      -- * No-storage transformer
    , VoidStorageT (..)
    )
  where

import           Control.Monad.Catch
import           Control.Monad.State

import           Data.Typeable

import           Data.Tree.AVL.Internal

-- | It is IO, so you can run any AVL operations in it.
--
--   Just don't expect that it will succeed in 'retrieve'.
type VoidStorage = IO

-- | Wrapper. Cancels any capabilities of @m@ to store stuff.
newtype VoidStorageT m a = VoidStorageT { runVoidStorageT :: m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

instance MonadTrans VoidStorageT where
    lift = VoidStorageT

instance (Show k, Typeable k) => KVRetrieve k n VoidStorage where
    retrieve k = throwM (NotFound k)

instance (Show k, Typeable k, MonadCatch m, MonadIO m) => KVRetrieve k n (VoidStorageT m) where
    retrieve k = throwM (NotFound k)
