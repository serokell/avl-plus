
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

instance (KVStoreMonad h k v m, Eq h, Show h, Typeable h, Hashable h, Show k, Show v) => KVStoreMonad h k v (HashMapStore h k v m) where
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

instance (Show a, Show b, Show c, Typeable a) => KVStoreMonad a b c NullStore where
    retrieve k = throwM (NotFound k)
    store _ _  = return ()

runWithCache :: KVStoreMonad h k v m => Storage h k v -> HashMapStore h k v m a -> m (a, Storage h k v)
runWithCache db action = runStateT action db

runOnEmptyCache :: KVStoreMonad h k v m => HashMapStore h k v m a -> m (a, Storage h k v)
runOnEmptyCache = runWithCache HM.empty

sandboxed :: KVStoreMonad h k v m => HashMapStore h k v NullStore a -> HashMapStore h k v m a
sandboxed action = do
    (res, _) <- liftIO $ runOnEmptyCache action
    return res

dumpDatabase :: (Show h, Show k, Show v, MonadIO m) => HashMapStore h k v m ()
dumpDatabase = do
    st <- get
    liftIO $ putStrLn (show st)
