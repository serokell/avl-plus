-- | Operation wrapper on AVL tree that mutates the storage
--   so it only keeps the last version.
module Data.Tree.AVL.Unsafe
    ( -- * Constraint to use
      Mutates

      -- * Constraint to implement
    , KVMutate (..)

      -- * Wrappers
    , mutateStorage
    , onMutableStorage
    )
  where

import Control.Lens ((^?), to)
import Control.Monad.Free

import Data.Maybe (fromMaybe)
import Data.Foldable (for_)
import Data.Monoid ((<>))
import qualified Data.Set as Set

import Data.Tree.AVL

-- | Allows for umpure storage of AVL that is rewritten on each write.
class (KVStore h node m, KVRetrieve h node m) => KVMutate h node m where
    root    :: m h        -- ^ Get current root of the tree
    setRoot :: h -> m ()  -- ^ Set current root of the tree
    erase   :: h -> m ()  -- ^ Remove node with given hash

-- | Enriches 'massStore'/'retrive' capabilities with 'erase' and
--   notion of single root.
type Mutates h k v m = (Base h k v m, KVMutate h (Isolated h k v) m)

contour :: Params h k v => Map h k v -> Set.Set h
contour = Set.fromList . go
  where
    go = \case
      Pure hash -> pure hash
      Free node -> children node >>= go

children :: MapLayer h k v c -> [c]
children node = do
    let ls = node^?mlLeft .to pure `orElse` []
    let rs = node^?mlRight.to pure `orElse` []
    ls <> rs

-- | Using 'KVMutate' capabilities, provide a 'Map'
onMutableStorage
    :: forall h k v m a
    .  Mutates h k v m
    => (Map h k v -> m a)
    -> m a
onMutableStorage query = do
    hash <- root @h @(Isolated h k v)
    query (ref hash)

-- | Retrieve root from storage, run @action@ on it,
--   calculate contour of resulting 'Map', 'save' the result
--   and delete all nodes between root (incl.) and the contour.
--
--   Oh, and also do 'setRoot' on new root.
mutateStorage
    :: forall h k v m
    .  Mutates h k v m
    => (Map h k v -> m (Map h k v))
    -> m ()
mutateStorage action = do
    hash   <- root @h @(Isolated h k v)
    result <- action (ref hash)
    let border = contour result
    new <- save result
    ref hash `eraseTo` border
    setRoot @h @(Isolated h k v) new
    return ()
  where
    eraseTo :: Map h k v -> Set.Set h -> m ()
    tree `eraseTo` set = if
        | rootSig `Set.member` set -> return ()
        | otherwise -> do
            cs <- flip loadAndM tree $ \node -> do
                erase @h @(Isolated h k v) rootSig
                return (children node)

            for_ cs $ \c -> c `eraseTo` set

      where
        rootSig
            = fromMaybe (error "mutateStorage: Storage is broken")
            $ rootHash tree



