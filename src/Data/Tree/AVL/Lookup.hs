module Data.Tree.AVL.Lookup where

import Control.Lens (use)
import Control.Monad.Trans.Class (lift)

import Data.Set (Set)
import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof
import Data.Tree.AVL.Zipper


lookup' :: Retrieves h k v m => k -> Map h k v -> m ((Maybe v, Set h), Map h k v)
lookup' k tree0 = do
    (mv, tree, trails) <- runZipped' (lookupZ k) UpdateMode tree0
    return ((mv, trails), tree)

lookup :: Retrieves h k v m => k -> Map h k v -> m ((Maybe v, Proof h k v), Map h k v)
lookup k tree0 = do
    (mv, tree, proof) <- runZipped (lookupZ k) UpdateMode tree0
    return ((mv, proof), tree)

lookupZ :: Retrieves h k v m => k -> Zipped h k v m (Maybe v)
lookupZ k = do
    goto (Plain k)
    tree  <- use locus
    lift (open tree) >>= \case
      MLLeaf {_mlKey, _mlValue} ->
        if _mlKey == Plain k
        then return (Just _mlValue)
        else return Nothing

      MLEmpty {} ->
        return Nothing

      _ ->
        error $ "lookup: `goto ended in non-terminal node"

