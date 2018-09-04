-- | Operation wrapper on AVL tree that mutates the storage
--   so it only keeps the last version.
module Data.Tree.AVL.Unsafe
    ( -- * Constraint to use
      Mutates

      -- * Constraint to implement
    , KVMutate (..)

      -- * Wrappers
    , overwrite

      -- * Methods
    , assignRoot
    , getRoot
    , contour
    )
  where

import Control.Lens ((^?), (^.), to)
import Control.Monad.Free

-- import Data.Maybe (fromMaybe)
import Data.Foldable (for_)
import Data.Traversable (for)
import Data.Monoid ((<>))
import qualified Data.Set as Set

import Data.Tree.AVL

-- import Debug.Trace as Debug

-- | Allows for umpure storage of AVL that is rewritten on each write.
class (KVStore h node m, KVRetrieve h node m) => KVMutate h node m where
    root    :: m h        -- ^ Get current root of the tree
    setRoot :: h -> m ()  -- ^ Set current root of the tree
    erase   :: h -> m ()  -- ^ Remove node with given hash

getRoot :: forall h k v m . Mutates h k v m => m h
getRoot = root @_ @(Isolated h k v)

assignRoot :: forall h k v m . Mutates h k v m => h -> m ()
assignRoot = setRoot @_ @(Isolated h k v)

eraseNode :: forall h k v m . Mutates h k v m => h -> m ()
eraseNode = erase @_ @(Isolated h k v)

-- | Enriches 'massStore'/'retrive' capabilities with 'erase' and
--   notion of single root.
type Mutates h k v m = (Base h k v m, KVMutate h (Isolated h k v) m)

contour :: forall h k v . Params h k v => Map h k v -> Set.Set h
contour = Set.fromList . go
  where
    go :: Map h k v -> [h]
    go = \case
      Pure hash -> pure hash
      Free node
        | Just hash <- node^.mlHash ->
            pure hash
        | otherwise ->
            children node >>= go

children :: MapLayer h k v c -> [c]
children node = do
    let ls = node^?mlLeft .to pure `orElse` []
    let rs = node^?mlRight.to pure `orElse` []
    ls <> rs

-- | Retrieves root from storage, runs @action@ on it,
--   calculates contour of resulting 'Map', 'save's the result
--   and deletes all nodes between root (incl.) and the contour.
--
--   Oh, and also does 'setRoot' on new root.
overwrite
    :: forall h k v m
    .  Mutates h k v m
    => Map h k v
    -> m (Proof h k v)
overwrite tree = do
    let border = contour tree
    let tree'  = assignHashes tree
    old       <- getRoot @h @k @v
    proofBody <- materialiseUpTo border (ref old)
    hash <- save tree'
    assignRoot @h @k @v hash
    remove proofBody
    return (Proof proofBody)
  where
    materialiseUpTo :: Set.Set h -> Map h k v -> m (Map h k v)
    materialiseUpTo border bush
      | unsafeRootHash bush `Set.member` border
        = return $ Pure (unsafeRootHash bush)

      | otherwise
        = flip loadAndM bush $ \layer -> do
            Free <$> for layer (materialiseUpTo border)

    remove = \case
      Pure _ ->
        return ()

      thunk@ (Free layer) -> do
        eraseNode @h @k @v (unsafeRootHash thunk)
        for_ (children layer) remove
