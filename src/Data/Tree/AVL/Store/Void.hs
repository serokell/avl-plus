-- | A stub storage that throws 'NotFound' on any 'retrieve'.

module Data.Tree.AVL.Store.Void
    ( -- * No-storage transformer.
      VoidStorageT (..)
      -- * No-storage monad.
    , VoidStorage
    ) where

import Control.Monad.Catch
import Control.Monad.State

import Data.Typeable

import Data.Tree.AVL.Internal

-- | Wrapper. Cancels any capabilities of @m@ to store stuff.
newtype VoidStorageT m a = VoidStorageT { runVoidStorageT :: m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

instance MonadTrans VoidStorageT where
    lift = VoidStorageT

instance (Show k, Typeable k, MonadCatch m, MonadIO m) => KVRetrieve k n (VoidStorageT m) where
    retrieve k = throwM (NotFound k)

-- | IO Wrapper that fails on any access attempt.
type VoidStorage = VoidStorageT IO
