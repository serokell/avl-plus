-- | Read operation.
--
--   Can be repeated on the proof it generates with the same result.

module Data.Tree.AVL.Lookup
    ( -- * Lookups
      Data.Tree.AVL.Lookup.lookup
    , lookupMany
    ) where

import Data.Traversable (for)
import Data.Set (Set)
import qualified Data.Set as Set (toList)
import qualified Data.Map as BinTree (Map, empty, singleton, unions)

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Zipper

-- | Retrieves value for given key. Also collects raw proof.
lookup :: Retrieves h k v m => k -> Map h k v -> m ((Maybe v, Set h), Map h k v)
lookup k tree0 = assoc <$> runZipped UpdateMode tree0 (lookupZ k)

-- | Retrieves value for given key. Also collects raw proof.
lookupMany :: Retrieves h k v m => Set k -> Map h k v -> m ((BinTree.Map k v, Set h), Map h k v)
lookupMany ks tree0 = (assoc <$>) $ runZipped UpdateMode tree0 $ do
    pairs <- for (Set.toList ks) $ \k -> do
        maybe BinTree.empty (BinTree.singleton k)
            <$> lookupZ k

    return $ BinTree.unions pairs

lookupZ :: Retrieves h k v m => k -> Zipped h k v m (Maybe v)
lookupZ k = do
    goto (Plain k)
    withLocus $ \case
        MLLeaf {_mlKey, _mlValue} -> do
            if   _mlKey == k
            then do
                gotoNextKey k
                gotoPrevKey k

                return $ Just _mlValue
            else do
                if _mlKey > k
                then gotoPrevKey k
                else gotoNextKey k

                return Nothing
        
        MLEmpty {} -> do
            return Nothing
        
        _ -> do 
            error $ "lookup: `goto ended in non-terminal node"

assoc :: (a, b, c) -> ((a, c), b)
assoc (mv, tree, trails) = ((mv, trails), tree)
