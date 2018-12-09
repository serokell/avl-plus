-- | Operation wrapper on AVL tree that mutates the storage
--   so it only keeps the last version.
module Data.Tree.AVL.Unsafe
    ( -- * Constraint to use
      Mutates

      -- * Constraint to implement
    , KVMutate (..)

      -- * Wrappers
    , overwrite
    , append

      -- * Methods
    , currentRoot
    , NoRootExists(..)

    , initialiseStorageIfNotAlready
    ) where

import Control.Exception (Exception)
import Lens.Micro.Platform (to, (^.), (^?))
import Control.Monad (void, when)
import Control.Monad.Catch (catch)
import Control.Monad.Free (Free (..))

import Data.Foldable (for_)
import Data.Monoid ((<>))
import qualified Data.Set as Set
import Data.Typeable (Typeable)

import Data.Tree.AVL.Insertion as AVL
import Data.Tree.AVL.Internal


-- | Provides possibility to use impure storage that is rewritten on
-- the each write.
class (KVStore h node m, KVRetrieve h node m) => KVMutate h node m where
    getRoot :: m h        -- ^ Get current root of the tree
    setRoot :: h -> m ()  -- ^ Set current root of the tree
    erase   :: h -> m ()  -- ^ Remove node with given hash

-- | Exception to be thrown by storage, if 'getRoot' impl can't
--   return current root.
data NoRootExists = NoRootExists
    deriving (Show, Typeable)

instance Exception NoRootExists

-- | Returns current root from storage.
currentRoot :: forall h k v m . Mutates h k v m => m (Map h k v)
currentRoot = ref <$> getRoot @_ @(Isolated h k v)

assignRoot :: forall h k v m . Mutates h k v m => Map h k v -> m ()
assignRoot new = setRoot @_ @(Isolated h k v) (unsafeRootHash new)

eraseTopNode :: forall h k v m . Mutates h k v m => Map h k v -> m ()
eraseTopNode = erase @_ @(Isolated h k v) . unsafeRootHash

-- | Enriches 'massStore'/'retrive' capabilities with 'erase' and a
--   notion of single root.
type Mutates h k v m = (Base h k v m, KVMutate h (Isolated h k v) m)

-- | Retrieve hashes of nearest subtrees that weren't materialised.
--
--   We will later remove all nodes that transitively refer to
--   these [non-materialised] nodes.
contour :: forall h k v . Params h k v => Map h k v -> Set.Set h
contour = Set.fromList . go
  where
    go :: Map h k v -> [h]
    go = \case
      Pure hash -> pure hash
      Free node
        | Just hash <- node^.mlHash -> pure hash
        | otherwise                 -> children node >>= go

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
    .  Mutates h k v m
    => Map h k v
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
            when (unsafeRootHash tree `Set.notMember` border) $ do
                eraseTopNode @h @k @v tree
                for_ (children layer) go


-- | Stores tree in the storage alongside whatever is there and
--   sets the root pointer to its root.
append
    :: forall h k v m
    .  Mutates h k v m
    => Map h k v
    -> m ()
append tree = assignRoot =<< save tree

-- | Initialises storage with a given set of kv-pairs,
--   if root is not present there.
initialiseStorageIfNotAlready
    :: forall h k v m
    .  Mutates h k v m
    => [(k, v)]
    -> m ()
initialiseStorageIfNotAlready kvs = do
    void (currentRoot @h @k @v) `catch` \NoRootExists -> do
        append (empty @h @k @v)
        overwrite  =<< AVL.fromList @h @k @v kvs
