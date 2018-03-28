
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE ExplicitForAll       #-}

module Data.Tree.AVL.RocksDBStore where

import Control.Concurrent.STM

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Catch

import Data.Binary
import Data.Default
import Data.Hashable
import Data.HashMap.Strict as HM
import Data.Typeable

import Database.RocksDB as RDB

import Data.Tree.AVL.Internal
import Data.Tree.AVL.KVStoreMonad
import Data.Tree.AVL.HashMapStore

type RocksDBStore = ReaderT DB IO

instance (Eq h, Typeable h, Hashable h, Show h, Show k, Show v, Binary h, Binary k, Binary v) => KVStoreMonad h k v RocksDBStore where
    retrieve k = do
        db   <- ask
        mres <- liftIO $ getBinary db def k
        case mres of
          Just it -> return it
          Nothing -> throwM (NotFound k)

    store k v = do
        error "Attempt to perform a single-value write"

type RDBM h k v = HashMapStore h k v RocksDBStore

transacted :: Stores h k v RocksDBStore => RDBM h k v a -> RocksDBStore a
transacted action = do
    (res, cache) <- runOnEmptyCache action
    return res

runRocksDBWithCache :: forall h k v a . Stores h k v (RDBM h k v) => FilePath -> RDBM h k v a -> IO a
runRocksDBWithCache dbName action = do
    bracket (RDB.open dbName def) RDB.close $ \db -> do
        (res, writes) <- runOnEmptyCache action `runReaderT` db
        massStore db writes
        return res

massStore :: Stores h k v RocksDBStore => DB -> Storage h k v -> IO ()
massStore db storage = do
    let pairs = HM.toList storage
    write db def $ put <$> pairs
  where
    put (k, v) = Put (binaryToBS k) (binaryToBS v)