-- | Read operation.
--
--   Can be repeated on the proof it generates with the same result.

module Data.Tree.AVL.Lookup
    ( -- * Lookups
      Data.Tree.AVL.Lookup.lookup
    , lookup'
    ) where

import Data.Set (Set)

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof
import Data.Tree.AVL.Zipper

-- | Retrieves value for given key. Also collects raw proof.
lookup :: Retrieves h k v m => k -> Map h k v -> m ((Maybe v, Set Revision), Map h k v)
lookup k tree0 = do
    (mv, tree, trails) <- runZipped (lookupZ k) UpdateMode tree0
    return ((mv, trails), tree)

-- | Retrieves value for given key. Also constructs baked proof.
lookup' :: Retrieves h k v m => k -> Map h k v -> m ((Maybe v, Proof h k v), Map h k v)
lookup' k tree0 = do
    (mv, tree, proof) <- runZipped' (lookupZ k) UpdateMode tree0
    return ((mv, proof), tree)

-- | The implementation of lookup.
lookupZ :: Retrieves h k v m => k -> Zipped h k v m (Maybe v)
lookupZ k = do
    goto (Plain k)
    withLocus $ \case
        MLLeaf {_mlKey, _mlValue} ->
            return $ if _mlKey == Plain k then Just _mlValue else Nothing
        MLEmpty {} -> return Nothing
        _ -> error $ "lookup: `goto ended in non-terminal node"
