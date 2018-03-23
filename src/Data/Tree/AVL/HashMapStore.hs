
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Data.Tree.AVL.HashMapStore where

import Control.Concurrent.STM

import Control.Monad.State
import Control.Monad.Catch

import Data.Hashable
import Data.HashMap.Strict as HM
import Data.Typeable

import Data.Tree.AVL.Internal
import Data.Tree.AVL.KVStoreMonad

type NullStore = IO

type Storage h k v = HashMap h (MapLayer h k v h)

type HashMapStore h k v = StateT (Storage h k v)

instance (KVStoreMonad m h (MapLayer h k v h), MonadCatch m, MonadIO m, Eq h, Show h, Show k, Show v, Typeable h, Hashable h) => KVStoreMonad (HashMapStore h k v m) h (MapLayer h k v h) where
    retrieve k = do
        --liftIO $ print "retrieve"
        mapping <- get
        case k `HM.lookup` mapping of
          Nothing -> do
            v <- lift $ retrieve k
            store k v
            return v
          
          Just it -> do
            return it

    store k v = do
        --liftIO $ putStrLn $ "store " ++ show k ++ " " ++ show v
        modify $ insert k v 

instance (Show a, Typeable a) => KVStoreMonad NullStore a b where
    retrieve k = throwM (NotFound k)
    store _ _  = return ()

runPureCache :: MonadIO m => Storage h k v -> HashMapStore h k v m a -> m (a, Storage h k v)
runPureCache db action = runStateT action db

runEmptyCache :: MonadIO m => HashMapStore h k v m a -> m (a, Storage h k v)
runEmptyCache = runPureCache HM.empty

withCacheLayer :: MonadIO m => Storage h k v -> HashMapStore h k v m a -> HashMapStore h k v m a
withCacheLayer db action = do
    (res, _) <- lift $ runPureCache db action
    return res

dumpDatabase :: (Show h, Show k, Show v, MonadIO m) => HashMapStore h k v m ()
dumpDatabase = do
    st <- get
    liftIO $ putStrLn (show st) 

--instance Stores h k v m => Stores h k v (HashMapStore h k v m)