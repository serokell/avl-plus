
module Data.Tree.AVL.Store.Pure.Sharded where

import Control.Concurrent.STM (TVar)
import Control.Monad.Reader (ReaderT, asks, MonadTrans (lift))

import qualified Data.Map as Map

import qualified Data.Tree.AVL.Store.Pure as Shard

type State h k v = Map.Map String (TVar (Shard.State h k v))

-- | ReaderT over a TVar accessible 'State'.
type StoreT h k v = ReaderT (State h k v)

withShard :: Monad m => String -> Shard.StoreT h k v m a -> StoreT h k v m a
withShard name action = do
  asks (Map.lookup name) >>= \case
    Nothing    -> error $ "shard `" ++ name ++ "` does not exist"
    Just state -> lift $ Shard.runStoreT state action