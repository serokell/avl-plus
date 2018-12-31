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
lookup :: Retrieves h k v m => k -> Map h k v -> m ((Maybe v, Set h), Map h k v)
lookup k tree0 = do
    (mv, tree, trails) <- runZipped UpdateMode tree0 $ lookupZ k
    return ((mv, trails), tree)

-- | Retrieves value for given key. Also constructs baked proof.
lookup' :: Retrieves h k v m => k -> Map h k v -> m ((Maybe v, Proof h k v), Map h k v)
lookup' k tree0 = do
    (mv, tree, proof) <- runZipped' UpdateMode tree0 $ lookupZ k
    return ((mv, proof), tree)

-- | The implementation of lookup.
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
