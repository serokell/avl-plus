-- | Pure in-memory storage for AVL tree.
module Data.Tree.AVL.Store.Pure
    ( -- * Carrier monad
      Store
    , State

      -- * Runner
    , run
    , dump
    , newPureState
    , clean
    )
  where

import Control.Concurrent.STM (TVar, readTVar, writeTVar, atomically, newTVarIO)
import Control.Lens (makeLenses, use, uses, (.=), (%=))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (throwM)
import Control.Monad.State (StateT, runStateT, put)
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Control.Monad (unless)

import Data.Monoid ((<>))
import qualified Data.Map as Map

import Data.Tree.AVL.Internal as AVL
import Data.Tree.AVL.Unsafe

-- import Debug.Trace as Debug

data State h k v = State
    { _psStorage :: Map.Map h (Isolated h k v)
    , _psRoot    :: Maybe h
    }

makeLenses ''State

-- | Monad
type Store h k v = ReaderT (TVar (State h k v))

asState :: MonadIO m => StateT (State h k v) m a -> Store h k v m a
asState action = do
    var <- ask
    st  <- liftIO $ atomically $ readTVar var
    (b, st') <- lift $ runStateT action st
    liftIO $ atomically $ writeTVar var st'
    return b

instance (Base h k v m, MonadIO m) => KVRetrieve h (Isolated h k v) (Store h k v m) where
    retrieve k = asState $ do
        -- st <- use psStorage
        uses psStorage (Map.lookup k) >>= maybe (throwM $ NotFound k) pure

instance (Base h k v m, MonadIO m) => KVStore h (Isolated h k v) (Store h k v m) where
    massStore pairs = asState $ do
        st <- use psStorage
        psStorage %= (<> Map.fromList pairs)

instance (Base h k v m, MonadIO m) => KVMutate h (Isolated h k v) (Store h k v m) where
    getRoot = asState $ do
        use psRoot >>= \case
          Just root -> return root
          Nothing   -> throwM NoRootExists

    setRoot new = asState $ psRoot .= Just new
    erase hash  = asState $ psStorage %= Map.delete hash

-- | Unlifts 'Store' monad into 'Base' one.
run :: forall h k v m a . (Params h k v, Monad m) => TVar (State h k v) -> Store h k v m a -> m a
run = flip runReaderT

newPureState :: forall h k v m . (Params h k v, MonadIO m) => m (TVar (State h k v))
newPureState = liftIO $ newTVarIO emptyPureState

emptyPureState :: forall h k v . Params h k v => State h k v
emptyPureState = State
    { _psStorage = Map.empty -- singleton monoHash (MLEmpty 0 (Just monoHash))
    , _psRoot    = Nothing
    }

-- | Dumps storage into console with given message.
dump :: forall h k v m . (MonadIO m, Base h k v m) => String -> Store h k v m ()
dump msg = asState $ do
    State storage root <- use id
    liftIO $ do
        putStrLn msg
        print root
        print storage
        putStrLn ""

clean :: forall h k v m . Base h k v m => Store h k v m ()
clean = asState $ put emptyPureState
