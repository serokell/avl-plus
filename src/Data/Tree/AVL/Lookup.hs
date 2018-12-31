-- | Read operation.
--
--   Can be repeated on the proof it generates with the same result.

module Data.Tree.AVL.Lookup
    ( -- * Lookups
      Data.Tree.AVL.Lookup.lookup
    ) where

import Data.Set (Set)

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Zipper

-- | Retrieves value for given key. Also collects raw proof.
lookup :: Retrieves h k v m => k -> Map h k v -> m ((Maybe v, Set h), Map h k v)
lookup k tree0 = (assoc <$>) $ runZipped UpdateMode tree0 $ do
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
  where
    assoc (mv, tree, trails) = ((mv, trails), tree)
