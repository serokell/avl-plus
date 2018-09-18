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
    , currentRoot
    , NoRootExists(..)

    , initialiseStorageIfNotAlready
    )
  where

import Control.Exception (Exception, SomeException)
import Control.Lens ((^?), (^.), to)
import Control.Monad.Catch (catch, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Free (Free (..))
import Control.Monad (when, void)

-- import Data.Maybe (fromMaybe)
import Data.Foldable (for_)
import Data.Monoid ((<>))
import qualified Data.Set as Set
import Data.Typeable (Typeable)

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Insertion as AVL

-- import Debug.Trace as Debug

-- | Allows for umpure storage of AVL that is rewritten on each write.
class (KVStore h node m, KVRetrieve h node m) => KVMutate h node m where
    getRoot :: m h        -- ^ Get current root of the tree
    setRoot :: h -> m ()  -- ^ Set current root of the tree
    erase   :: h -> m ()  -- ^ Remove node with given hash

data NoRootExists = NoRootExists
    deriving (Show, Typeable)

instance Exception NoRootExists

-- | Returns current root from storage.
currentRoot :: forall h k v m . Mutates h k v m => m (Map h k v)
currentRoot = ref <$> getRoot @_ @(Isolated h k v)

assignRoot :: forall h k v m . Mutates h k v m => Map h k v -> m ()
assignRoot new = do
    setRoot @_ @(Isolated h k v) (unsafeRootHash new)

eraseTopNode :: forall h k v m . Mutates h k v m => Map h k v -> m ()
eraseTopNode = erase @_ @(Isolated h k v) . unsafeRootHash

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
        | Just hash <- node^.mlHash -> pure hash
        | otherwise                 -> children node >>= go

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
    -> m ()
overwrite tree' = do
    p "overwrite"
    pr tree'
    removeTo (contour tree') =<< currentRoot
    p "removed up to contour"
    assignRoot               =<< save tree'
    p "saved, root is assigned"
    return ()
  `catch` \(e :: SomeException) -> do
    p "E X C E P T I O N during overwrite:"
    pr e
    throwM e
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

p :: MonadIO m => String -> m ()
p = liftIO . putStrLn

pr :: (MonadIO m, Show a) => a -> m ()
pr = liftIO . print

initialiseStorageIfNotAlready
    :: forall h k v m
    .  Mutates h k v m
    => [(k, v)]
    -> m ()
initialiseStorageIfNotAlready kvs = do
    p "optional init..."
    void (currentRoot @h @k @v) `catch` \NoRootExists -> do
        p "...got exn, init"
        tree <- AVL.fromList @h @k @v kvs
        p "...got tree"
        pr tree
        assignRoot (assignHashes $ empty @h @k @v)
        p "...assigned"
        overwrite @h @k @v tree
        p "...overwriten."
    p "...done."
