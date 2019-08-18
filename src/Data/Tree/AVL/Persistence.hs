-- | Operation wrapper on AVL tree that mutates the storage
--   so it only keeps the last version.
module Data.Tree.AVL.Persistence
    ( -- * Constraint to use
      Erases  (..)
    , Appends (..)

      -- * Wrappers
    , overwrite
    , append
    , save

      -- * Methods
    , currentRoot
    , NoRootExists (..)

    , genesis
    ) where

import Control.Exception (Exception)
import Control.Monad (void, when)
import Control.Monad.Catch (catch)
import Control.Monad.Free (Free (..))
import Control.Lens (to, (^?))

import Data.Foldable (for_)
import Data.Monoid ((<>))
import qualified Data.Set as Set

import Data.Tree.AVL.Insertion as AVL
import Data.Tree.AVL.Internal


-- | Provides possibility to use impure storage that is rewritten on
-- the each write.
class
    ( Retrieves h k v m
    )
  =>
    Appends h k v m
      | m -> h k v
  where
    getRoot   :: m h        -- ^ Get current root of the tree
    setRoot   :: h -> m ()  -- ^ Set current root of the tree
    massStore :: [(h, Rep h k v)] -> m ()

class Appends h k v m => Erases h k v m | m -> h k v where
    erase :: h -> m ()  -- ^ Remove node with given hash

-- | Exception to be thrown by storage, if 'getRoot' impl can't
--   return current root.
data NoRootExists = NoRootExists
    deriving (Show)

instance Exception NoRootExists

-- | Recursively store all the materialized nodes in the database.
save :: forall h k v m . Appends h k v m => Map h k v -> m (Map h k v)
save tree = do
    massStore $ collect tree
    return (ref (rootHash tree))
  where
    -- | Turns a 'Map' into relation of (hash, isolated-node),
    --   to use in 'save'.
    collect :: Map h k v -> [(h, Rep h k v)]
    collect it = case it of
        Pure _     -> []
        Free layer -> do
            let hash  = rootHash it
            let node  = toRep $ (layer :: MapLayer h k v (Map h k v))
            let left  = layer^?mlLeft .to collect `orElse` []
            let right = layer^?mlRight.to collect `orElse` []

            [(hash, node)] ++ left ++ right

-- | Returns current root from storage.
currentRoot :: forall h k v m . Appends h k v m => m (Map h k v)
currentRoot = ref <$> getRoot

assignRoot :: forall h k v m . Appends h k v m => Map h k v -> m ()
assignRoot = setRoot . rootHash

eraseTopNode :: forall h k v m . Erases h k v m => Map h k v -> m ()
eraseTopNode = erase . rootHash

-- | Retrieve hashes of nearest subtrees that weren't materialised.
--
--   We will later remove all nodes that transitively refer to
--   these [non-materialised] nodes.
contour :: forall h k v . (Hash h k v, Ord h) => Map h k v -> Set.Set h
contour = Set.fromList . go
  where
    go :: Map h k v -> [h]
    go = \case
      Pure hash -> pure hash
      Free node -> children node >>= go

children :: MapLayer h k v c -> [c]
children node = do
    let ls = node^?mlLeft .to pure `orElse` []
    let rs = node^?mlRight.to pure `orElse` []
    ls <> rs

-- | Retrieves root from the storage, runs @action@ on it,
--   calculates contour of the resulting 'Map', 'save's the result
--   and deletes all the nodes between root (incl.) and the contour.
--
--   Then, it does 'setRoot' on the new root.
overwrite
    :: forall h k v m
    .  Erases h k v m
    => Map    h k v
    -> m ()
overwrite tree' = do
    removeTo (contour tree') =<< currentRoot
    append tree'
  where
    removeTo :: Set.Set h -> Map h k v -> m ()
    removeTo border = go
      where
        go :: Map h k v -> m ()
        go tree = do
            layer <- load tree
            when (rootHash tree `Set.notMember` border) $ do
                eraseTopNode @h @k @v tree
                for_ (children layer) go


-- | Stores tree in the storage alongside whatever is there and
--   sets the root pointer to its root.
append
    :: forall  h k v m
    .  Appends h k v m
    => Map     h k v
    -> m ()
append tree = assignRoot =<< save tree

-- | Initialises storage with a given set of kv-pairs,
--   if root is not present there.
genesis
    :: forall  h k v m
    .  Appends h k v m
    => [(k, v)]
    -> m ()
genesis kvs = do
    void currentRoot `catch` \NoRootExists -> do
        append (empty @h @k @v)
        append =<< AVL.fromList kvs
