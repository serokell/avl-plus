
{-# language DefaultSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language FlexibleInstances #-}
{-# language TypeFamilies #-}

module Data.Tree.AVL.KVStoreMonad where

--import Universum

import Control.Monad.Catch
import Control.Monad.Trans.Class

import Data.Composition

class MonadCatch m => KVStoreMonad m k v where
    retrieve :: k -> m v
    store    :: k -> v -> m ()

--exists :: KVStoreMonad m k v => k -> m Bool
--exists k = do
--    _ <- retrieve k :: m v
--    return True
--  `catch` \(SomeException e) ->
--    return False

instance (MonadTrans t, MonadCatch (t m), KVStoreMonad m k v) => KVStoreMonad (t m) k v where

    retrieve = lift .  retrieve
    store    = lift .: store