-- | Pure in-memory storage for AVL tree.
module Data.Tree.AVL.Store.Pure
    ( -- * Carrier monad
      Store

      -- * Runner
    , run
    )
  where

import Control.Lens (makeLenses, use, uses, (.=), (%=))
import Control.Monad.Catch (throwM)
import Control.Monad.State (StateT, evalStateT)

import Data.Monoid ((<>))
import Data.Map as Map
import Data.Maybe (fromJust)

import Data.Tree.AVL.Internal as AVL
import Data.Tree.AVL.Unsafe

import Debug.Trace as Debug

data State h k v = State
    { _psStorage :: Map.Map h (Isolated h k v)
    , _psRoot    :: h
    }

makeLenses ''State

-- | Monad
type Store h k v m = StateT (State h k v) m

instance Base h k v m => KVRetrieve h (Isolated h k v) (Store h k v m) where
    retrieve k = do
        st <- use psStorage
        (if k `Map.notMember` st then Debug.traceShow (k, "?", st) else id) $
            uses psStorage (Map.lookup k) >>= maybe (throwM $ NotFound k) pure

instance Base h k v m => KVStore h (Isolated h k v) (Store h k v m) where
    massStore pairs = do
        psStorage %= (<> Map.fromList pairs)

instance Base h k v m => KVMutate h (Isolated h k v) (Store h k v m) where
    root        = use psRoot
    setRoot new = psRoot .= new
    erase hash  = psStorage %= Map.delete hash

-- | Unlifts 'Store' monad into 'Base' one.
run :: forall h k v m a . Base h k v m => Store h k v m a -> m a
run = flip evalStateT State
    { _psStorage = Map.empty
    , _psRoot
        = fromJust
        $ rootHash
        $ assignHashes (AVL.empty @h @k @v)
    }
