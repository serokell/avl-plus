
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications       #-}

module Data.Tree.AVL.HashMapStore where

import Control.Monad.State
import Control.Monad.Catch

import Data.ByteString (ByteString)
import Data.Hashable
import Data.HashMap.Strict as HM
import Data.Typeable

import Data.Tree.AVL.Internal

type NullStore = IO

type Storage k = HashMap k ByteString

type HashMapStore k = StateT (Storage k)

instance (Serialisable k, KVStoreMonad k m, Eq k, Typeable k, Hashable k, Show k) => KVStoreMonad k (HashMapStore k m) where
    retrieve k = do
        --liftIO $ print "retrieve"
        mapping <- get
        case k `HM.lookup` mapping >>= deserialise of
          Nothing -> do
            v <- lift $ retrieve k
            store k v
            return v

          Just it -> do
            return it

    store k v = do
        --liftIO $ putStrLn $ "store " ++ show k ++ " " ++ show v
        modify $ insert k (serialise v)

instance (Show k, Typeable k, Serialisable k) => KVStoreMonad k NullStore where
    retrieve k = throwM (NotFound k)
    store _ _  = return ()

runWithCache :: forall k m a. KVStoreMonad k m => Storage k -> HashMapStore k m a -> m (a, Storage k)
runWithCache db action = runStateT action db

runOnEmptyCache :: forall k m a. KVStoreMonad k m => HashMapStore k m a -> m (a, Storage k)
runOnEmptyCache = runWithCache HM.empty

sandboxed :: forall k m a. (Show k, Eq k, Typeable k, KVStoreMonad k m) => HashMapStore k NullStore a -> HashMapStore k m a
sandboxed action = do
    (res, _) <- liftIO $ runOnEmptyCache action
    return res

dumpDatabase :: (Show k, MonadIO m) => HashMapStore k m ()
dumpDatabase = do
    st <- get
    liftIO $ putStrLn (show st)
