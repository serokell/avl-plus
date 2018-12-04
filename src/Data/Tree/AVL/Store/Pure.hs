-- | Pure in-memory storage for AVL tree.
module Data.Tree.AVL.Store.Pure
    ( -- * Carrier monad
      StoreT
    , State

      -- * Runner
    , runStoreT
    , dump
    , newState
    , clean
    ) where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Lens (makeLenses, use, uses, (%=), (.=))
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Control.Monad.State (StateT, put, runStateT)

import qualified Data.Map as Map
import Data.Monoid ((<>))

import Data.Tree.AVL.Internal as AVL
import Data.Tree.AVL.Unsafe

-- | Pure state containing avl changes as a regular 'Map'.
data State h k v = State
    { _psStorage :: Map.Map h (Isolated h k v)
    , _psRoot    :: Maybe h
    }

makeLenses ''State

-- | ReaderT over a TVar accessible 'State'.
type StoreT h k v = ReaderT (TVar (State h k v))

-- | Nicer way to assign things via lenses.
asState :: MonadIO m => StateT (State h k v) m a -> StoreT h k v m a
asState action = do
    var <- ask
    st  <- liftIO $ atomically $ readTVar var
    (b, st') <- lift $ runStateT action st
    liftIO $ atomically $ writeTVar var st'
    return b

instance (Base h k v m, MonadIO m) => KVRetrieve h (Isolated h k v) (StoreT h k v m) where
    retrieve k = asState $
        uses psStorage (Map.lookup k) >>= maybe (throwM $ NotFound k) pure

instance (Base h k v m, MonadIO m) => KVStore h (Isolated h k v) (StoreT h k v m) where
    massStore pairs = asState $ psStorage %= (<> Map.fromList pairs)

instance (Base h k v m, MonadIO m) => KVMutate h (Isolated h k v) (StoreT h k v m) where
    getRoot      = asState $ maybe (throwM NoRootExists) pure =<< use psRoot
    setRoot new  = asState $ psRoot    .= Just new
    erase   hash = asState $ psStorage %= Map.delete hash

-- | Unlifts 'StoreT' monad into 'Base' one.
runStoreT :: forall h k v m a. (Params h k v, Monad m)
    => TVar (State h k v)
    -> StoreT h k v m a
    -> m a
runStoreT = flip runReaderT

-- | Creates new empty state.
newState ::
       forall h k v m. (Params h k v, MonadIO m)
    => m (TVar (State h k v))
newState = liftIO $ newTVarIO emptyState

emptyState :: forall h k v . Params h k v => State h k v
emptyState = State
    { _psStorage = Map.empty -- singleton monoHash (MLEmpty 0 (Just monoHash))
    , _psRoot    = Nothing
    }

-- | Dumps storage into console with given message.
dump :: forall h k v m . (MonadIO m, Base h k v m) => String -> StoreT h k v m ()
dump msg = asState $ do
    State storage root <- use id
    liftIO $ do
        putStrLn msg
        print root
        print storage
        putStrLn ""

-- | Resets current state to empty.
clean :: forall h k v m . (MonadIO m, Base h k v m) => StoreT h k v m ()
clean = asState $ put emptyState
