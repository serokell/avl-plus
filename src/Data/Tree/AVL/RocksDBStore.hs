module Data.Tree.AVL.RocksDBStore where

import Control.Monad.Catch (bracket, throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State ()

import Data.Default (def)
import Data.Hashable (Hashable)
import Data.HashMap.Strict as HM (toList)
import Data.Typeable (Typeable)

import Database.RocksDB as RDB (BatchOp (Put), DB, close, get, open, write)

import Data.Tree.AVL.HashMapStore
import Data.Tree.AVL.Internal

type RocksDBStore = ReaderT DB IO

instance (Eq h, Typeable h, Hashable h, Show h, Serialisable h) => KVStoreMonad h RocksDBStore where
    retrieve k = do
        db   <- ask
        mres <- liftIO $ get db def (serialise k)
        case mres of
          Just it ->
            case deserialise it of
              Left err -> do
                throwM (DeserialisationError err)

              Right res -> do
                return res

          Nothing -> throwM (NotFound k)

    store _k _v = do
        error "Attempt to perform a single-value write"

type RDBM h = HashMapStore h RocksDBStore

transacted :: (Eq h, Show h, Serialisable h, Typeable h, Hashable h) => RDBM h a -> RocksDBStore a
transacted action = do
    (res, cache) <- runOnEmptyCache action
    massStore cache
    return res

runRocksDBWithCache :: FilePath -> RocksDBStore a -> IO a
runRocksDBWithCache dbName action = do
    bracket (RDB.open dbName def) RDB.close $ runReaderT action

massStore :: Serialisable h => Storage h -> RocksDBStore ()
massStore storage = do
    let pairs = HM.toList storage
    db <- ask
    liftIO $ write db def $ put <$> pairs
  where
    put (k, v) = Put (serialise k) v
