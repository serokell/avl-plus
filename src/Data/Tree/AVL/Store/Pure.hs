-- | Pure in-memory storage for AVL tree.
module Data.Tree.AVL.Store.Pure
    ( -- * Carrier monad
      Store

      -- * Runner
    , run
    , dump
    )
  where

import Control.Lens (makeLenses, use, uses, (.=), (%=))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (throwM)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad (unless)

import Data.Monoid ((<>))
import qualified Data.Map as Map

import Data.Tree.AVL.Internal as AVL
import Data.Tree.AVL.Unsafe

-- import Debug.Trace as Debug

data State h k v = State
    { _psStorage :: Map.Map h (Isolated h k v)
    , _psRoot    :: h
    }

makeLenses ''State

-- | Monad
type Store h k v m = StateT (State h k v) m

instance Base h k v m => KVRetrieve h (Isolated h k v) (Store h k v m) where
    retrieve k = do
        -- st <- use psStorage
        uses psStorage (Map.lookup k) >>= maybe (throwM $ NotFound k) pure

instance Base h k v m => KVStore h (Isolated h k v) (Store h k v m) where
    massStore pairs = do
        st <- use psStorage

        unless (null st) $ do
            error $ "massStore/st:  " ++ show st
               ++ "\nmassStore/st': " ++ show (st <> Map.fromList pairs)

        psStorage %= (<> Map.fromList pairs)

instance Base h k v m => KVMutate h (Isolated h k v) (Store h k v m) where
    getRoot     = use psRoot
    setRoot new = psRoot .= new
    erase hash  = psStorage %= Map.delete hash

-- | Unlifts 'Store' monad into 'Base' one.
run :: forall h k v m a . Base h k v m => Store h k v m a -> m a
run = flip evalStateT State
    { _psStorage = Map.singleton monoHash (MLEmpty 0 (Just monoHash))
    , _psRoot    = monoHash
    }
  where
    monoHash = emptyHash @_ @k @v

-- | Dumps storage into console with given message.
dump :: forall h k v m . (MonadIO m, Base h k v m) => String -> Store h k v m ()
dump msg = do
    State storage root <- use id
    liftIO $ do
        putStrLn msg
        print root
        print storage
        putStrLn ""
