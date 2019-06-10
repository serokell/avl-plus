-- | Operation wrapper on AVL tree that mutates the storage
--   so it only keeps the last version.
module Data.Tree.AVL.Persistence
    ( -- * Constraint to use
      Overwrites
    , Appends

      -- * Constraint to implement
    , KVOverwrite (..)
    , KVAppend    (..)

      -- * Wrappers
    , overwrite
    , append

      -- * Methods
    , currentRoot
    , NoRootExists (..)

    , initialiseStorageIfNotAlready
    ) where

import Control.Exception (Exception)
import Control.Monad (void, when)
import Control.Monad.Catch (catch)
import Control.Monad.Free (Free (..))
import Lens.Micro.Platform (to, (^?))

import Data.Foldable (for_)
import Data.Monoid ((<>))
import qualified Data.Set as Set

import Data.Tree.AVL.Insertion as AVL
import Data.Tree.AVL.Internal


-- | Provides possibility to use impure storage that is rewritten on
-- the each write.
class
    ( KVStore    h k v m
    , KVRetrieve h k v m
    )
  =>
    KVAppend h k v m
      | m -> h k v
  where
    getRoot :: m h        -- ^ Get current root of the tree
    setRoot :: h -> m ()  -- ^ Set current root of the tree

class KVAppend h k v m => KVOverwrite h k v m | m -> h k v where
    erase :: h -> m ()  -- ^ Remove node with given hash

-- | Exception to be thrown by storage, if 'getRoot' impl can't
--   return current root.
data NoRootExists = NoRootExists
    deriving (Show)

instance Exception NoRootExists

-- | Returns current root from storage.
currentRoot :: forall h k v m . Appends h k v m => m (Map h k v)
currentRoot = ref <$> getRoot

assignRoot :: forall h k v m . Appends h k v m => Map h k v -> m ()
assignRoot = setRoot . rootHash

eraseTopNode :: forall h k v m . Overwrites h k v m => Map h k v -> m ()
eraseTopNode = erase . rootHash

-- | Enriches 'massStore'/'retrive' capabilities with 'erase' and a
--   notion of single root.
type Overwrites h k v m =
    ( Base        h k v m
    , KVOverwrite h k v m
    )

type Appends h k v m =
    ( Base     h k v m
    , KVAppend h k v m
    )

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
    :: forall     h k v m
    .  Overwrites h k v m
    => Map        h k v
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
initialiseStorageIfNotAlready
    :: forall  h k v m
    .  Appends h k v m
    => [(k, v)]
    -> m ()
initialiseStorageIfNotAlready kvs = do
    void (currentRoot @h @k @v) `catch` \NoRootExists -> do
        append (empty @h @k @v)
        append =<< AVL.fromList @h @k @v kvs
