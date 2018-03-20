
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Data.Tree.AVL.HashMapStore where

import Control.Concurrent.STM

import Control.Monad.Reader
import Control.Monad.Catch

import Data.Hashable
import Data.HashMap.Strict as HM
import Data.Typeable

import Data.Tree.AVL.Internal
import Data.Tree.AVL.KVStoreMonad

type NullStore = IO

type Storage h k v = HashMap h (MapLayer h k v h)

type HashMapStore h k v = ReaderT (TVar (Storage h k v))

instance (KVStoreMonad m h (MapLayer h k v h), MonadCatch m, MonadIO m, Eq h, Show h, Typeable h, Hashable h) => KVStoreMonad (HashMapStore h k v m) h (MapLayer h k v h) where
    retrieve k = do
        liftIO $ print "retrieve"
        mapVar <- ask
        mapping <- liftIO $ atomically $ readTVar mapVar
        case k `HM.lookup` mapping of
          Nothing -> do
            v <- lift $ retrieve k
            store k v
            return v
          
          Just it -> do
            return it

    store k v = do
        liftIO $ print "store"
        mapVar <- ask
        liftIO $ atomically $ mapVar `modifyTVar` insert k v 

instance (Show a, Typeable a) => KVStoreMonad NullStore a b where
    retrieve k = throwM (NotFound k)
    store _ _  = return ()

runPureCache :: MonadIO m => Storage h k v -> HashMapStore h k v m a -> m (a, Storage h k v)
runPureCache db action = do
    cache <- liftIO $ newTVarIO db
    a     <- runReaderT action cache
    after <- liftIO $ atomically $ readTVar cache
    return (a, after)

runEmptyCache :: MonadIO m => HashMapStore h k v m a -> m (a, Storage h k v)
runEmptyCache = runPureCache HM.empty