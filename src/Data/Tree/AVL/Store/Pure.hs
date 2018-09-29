-- | Pure in-memory storage for AVL tree.
module Data.Tree.AVL.Store.Pure
    ( -- * Carrier monad
      PureStore
    , PureState

      -- * Runner
    , run
    , dump
    , newPureState
    , clean
    )
  where

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
data PureState h k v = PureState
    { _psStorage :: Map.Map h (Isolated h k v)
    , _psRoot    :: Maybe h
    }

makeLenses ''PureState

-- | ReaderT over a TVar accessible 'PureState'.
type PureStore h k v = ReaderT (TVar (PureState h k v))

asPureState :: MonadIO m => StateT (PureState h k v) m a -> PureStore h k v m a
asPureState action = do
    var <- ask
    st  <- liftIO $ atomically $ readTVar var
    (b, st') <- lift $ runStateT action st
    liftIO $ atomically $ writeTVar var st'
    return b

instance (Base h k v m, MonadIO m) => KVRetrieve h (Isolated h k v) (PureStore h k v m) where
    retrieve k = asPureState $
        uses psStorage (Map.lookup k) >>= maybe (throwM $ NotFound k) pure

instance (Base h k v m, MonadIO m) => KVStore h (Isolated h k v) (PureStore h k v m) where
    massStore pairs = asPureState $ psStorage %= (<> Map.fromList pairs)

instance (Base h k v m, MonadIO m) => KVMutate h (Isolated h k v) (PureStore h k v m) where
    getRoot = asPureState $ maybe (throwM NoRootExists) pure =<< use psRoot
    setRoot new = asPureState $ psRoot .= Just new
    erase hash  = asPureState $ psStorage %= Map.delete hash

-- | Unlifts 'PureStore' monad into 'Base' one.
run :: forall h k v m a. (Params h k v, Monad m)
    => TVar (PureState h k v)
    -> PureStore h k v m a
    -> m a
run = flip runReaderT

newPureState ::
       forall h k v m. (Params h k v, MonadIO m)
    => m (TVar (PureState h k v))
newPureState = liftIO $ newTVarIO emptyPureState

emptyPureState :: forall h k v . Params h k v => PureState h k v
emptyPureState = PureState
    { _psStorage = Map.empty -- singleton monoHash (MLEmpty 0 (Just monoHash))
    , _psRoot    = Nothing
    }

-- | Dumps storage into console with given message.
dump :: forall h k v m . (MonadIO m, Base h k v m) => String -> PureStore h k v m ()
dump msg = asPureState $ do
    PureState storage root <- use id
    liftIO $ do
        putStrLn msg
        print root
        print storage
        putStrLn ""

clean :: forall h k v m . Base h k v m => PureStore h k v m ()
clean = asPureState $ put emptyPureState
