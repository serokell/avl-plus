
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE ExplicitForAll       #-}

module Data.Tree.AVL.RocksDBStore where

import Control.Monad.State              ()
import Control.Monad.Reader             (ReaderT, runReaderT, ask)
import Control.Monad.Catch              (bracket, throwM)
import Control.Monad.IO.Class           (liftIO)

import Data.Binary                      (Binary)
import Data.Default                     (def)
import Data.Hashable                    (Hashable)
import Data.HashMap.Strict      as HM   (toList)
import Data.Typeable                    (Typeable)

import Database.RocksDB         as RDB  (DB, open, close, getBinary, BatchOp(Put), write, binaryToBS)

import Data.Tree.AVL.Internal
import Data.Tree.AVL.HashMapStore

type RocksDBStore = ReaderT DB IO

instance (Eq h, Typeable h, Hashable h, Show h, Binary h) => KVStoreMonad h RocksDBStore where
    retrieve k = do
        db   <- ask
        mres <- liftIO $ getBinary db def k
        case mres of
          Just it -> return it
          Nothing -> throwM (NotFound k)

    store _k _v = do
        error "Attempt to perform a single-value write"

type RDBM h = HashMapStore h RocksDBStore

transacted :: (Eq h, Show h, Binary h, Typeable h, Hashable h, KVStoreMonad h (RDBM h)) => RDBM h a -> RocksDBStore a
transacted action = do
    (res, cache) <- runOnEmptyCache action
    massStore cache
    return res

runRocksDBWithCache :: FilePath -> RocksDBStore a -> IO a
runRocksDBWithCache dbName action = do
    bracket (RDB.open dbName def) RDB.close $ runReaderT action

massStore :: (Binary h, KVStoreMonad h RocksDBStore) => Storage h -> RocksDBStore ()
massStore storage = do
    let pairs = HM.toList storage
    db <- ask
    liftIO $ write db def $ put <$> pairs
  where
    put (k, v) = Put (binaryToBS k) (binaryToBS v)