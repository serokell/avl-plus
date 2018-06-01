
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Tree.AVL.HashMapStore where

import Control.Monad.State
import Control.Monad.Catch

import Data.ByteString (ByteString)
import Data.Hashable
import Data.HashMap.Strict as HM
import Data.Typeable

import Data.Tree.AVL.Internal

type NullStore = IO

newtype NullStoreT m a = NullStoreT { runNullStoreT :: m a }
    deriving (Functor, Applicative, Monad, MonadThrow, MonadCatch)

type Storage k = HashMap k ByteString

type HashMapStore k = StateT (Storage k)

instance MonadTrans NullStoreT where
    lift = NullStoreT

instance (Serialisable k, KVStoreMonad k m, Eq k, Typeable k, Hashable k, Show k) => KVStoreMonad k (HashMapStore k m) where
    retrieve k = do
        --liftIO $ print "retrieve"
        mapping <- get
        case k `HM.lookup` mapping of
          Nothing -> do
            v <- lift $ retrieve k
            store k v
            return v

          Just it -> do
            case deserialise it of
              Left err -> do
                throwM (DeserialisationError err)

              Right res -> do
                return res

    store k v = do
        --liftIO $ putStrLn $ "store " ++ show k ++ " " ++ show v
        modify $ insert k (serialise v)

instance (Show k, Typeable k, Serialisable k) => KVStoreMonad k NullStore where
    retrieve k = throwM (NotFound k)
    store _ _  = return ()

instance (Show k, Typeable k, Serialisable k, MonadCatch m) => KVStoreMonad k (NullStoreT m) where
    retrieve k = throwM (NotFound k)
    store _ _  = return ()

runWithCache :: KVStoreMonad k m => Storage k -> HashMapStore k m a -> m (a, Storage k)
runWithCache db action = runStateT action db

runOnEmptyCache :: KVStoreMonad k m => HashMapStore k m a -> m (a, Storage k)
runOnEmptyCache = runWithCache HM.empty

sandboxedT :: (Show k, Eq k, Typeable k, Serialisable k, MonadCatch m) => HashMapStore k (NullStoreT m) a -> HashMapStore k m a
sandboxedT action = do
    (res, _) <- lift $ runNullStoreT $ runOnEmptyCache action
    return res

sandboxed :: (Show k, Eq k, Typeable k, Serialisable k, MonadIO m) => HashMapStore k NullStore a -> HashMapStore k m a
sandboxed action = do
    (res, _) <- liftIO $ runOnEmptyCache action
    return res

dumpDatabase :: (Show k, MonadIO m) => HashMapStore k m ()
dumpDatabase = do
    st <- get
    liftIO $ putStrLn (show st)
