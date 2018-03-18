
{-# language DefaultSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language FlexibleInstances #-}
{-# language TypeFamilies #-}

module Data.Tree.AVL.KVStoreMonad where

--import Universum

import Control.Applicative
import Control.Monad.Catch
--import Control.Monad.IO.Class

import Data.Foldable hiding (toList)
import Data.Typeable
--import Data.Hashable
import Data.HashMap.Strict (toList, HashMap)

--import System.IO.Unsafe

class (Alternative m, MonadCatch m) => KVStoreMonad m k v where
    retrieve :: k -> m v
    store    :: k -> v -> m ()

seed :: KVStoreMonad m k v => HashMap k v -> m ()
seed hm = toList hm `for_` uncurry store

--exists :: KVStoreMonad m k v => k -> m Bool
--exists k = do
--    _ <- retrieve k :: m v
--    return True
--  `catch` \(SomeException e) ->
--    return False

--instance (MonadTrans t, Alternative (t m), MonadCatch (t m), KVStoreMonad m k v) => KVStoreMonad (t m) k v where
--    retrieve = lift .  retrieve
--    store    = lift .: store

data NotFound k = NotFound k
    deriving (Show, Typeable)

instance (Show k, Typeable k) => Exception (NotFound k) where
