
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Data.Tree.AVL.HashMapStore where

import Control.Monad.State
import Control.Monad.Catch

import Data.Binary (decode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.Hashable
import Data.HashMap.Strict as HM
import Data.Typeable

import Data.Tree.AVL.Internal

type NullStore = IO

type Storage h = HashMap h ByteString

type HashMapStore h = StateT (Storage h)

instance (KVStoreMonad h m, Eq h, Typeable h, Hashable h, Show h) => KVStoreMonad h (HashMapStore h m) where
    retrieve k = do
        --liftIO $ print "retrieve"
        mapping <- get
        case k `HM.lookup` mapping of
          Nothing -> do
            v <- lift $ retrieve k
            store k v
            return v

          Just it -> do
            return (decode it)

    store k v = do
        --liftIO $ putStrLn $ "store " ++ show k ++ " " ++ show v
        modify $ insert k (encode v)

instance (Show a, Typeable a) => KVStoreMonad a NullStore where
    retrieve k = throwM (NotFound k)
    store _ _  = return ()

runWithCache :: KVStoreMonad h m => Storage h -> HashMapStore h m a -> m (a, Storage h)
runWithCache db action = runStateT action db

runOnEmptyCache :: KVStoreMonad h m => HashMapStore h m a -> m (a, Storage h)
runOnEmptyCache = runWithCache HM.empty

sandboxed :: (Show h, Eq h, Typeable h, KVStoreMonad h m) => HashMapStore h NullStore a -> HashMapStore h m a
sandboxed action = do
    (res, _) <- liftIO $ runOnEmptyCache action
    return res

dumpDatabase :: (Show h, MonadIO m) => HashMapStore h m ()
dumpDatabase = do
    st <- get
    liftIO $ putStrLn (show st)
