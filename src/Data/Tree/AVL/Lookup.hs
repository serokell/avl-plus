
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Tree.AVL.Lookup where

import Control.Lens (use)
import Control.Monad.Trans.Class (lift)

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof
import Data.Tree.AVL.Zipper

lookup' :: Stores h k v m => k -> Map h k v m -> m ((Maybe v, RevSet), Map h k v m)
lookup' k tree0 = do
    (mv, tree, trails) <- runZipped' (lookupZ k) UpdateMode tree0
    return ((mv, trails), tree)

lookup :: Stores h k v m => k -> Map h k v m -> m ((Maybe v, Proof h k v), Map h k v m)
lookup k tree0 = do
    (mv, tree, proof) <- runZipped (lookupZ k) UpdateMode tree0
    return ((mv, proof), tree)

lookupZ :: Stores h k v m => k -> Zipped h k v m (Maybe v)
lookupZ k = do
    goto k
    mark
    tree  <- use locus
    layer <- lift $ pick tree
    case layer of
      MLLeaf {_mlKey, _mlValue} ->
        if _mlKey == k
        then return (Just _mlValue)
        else return Nothing

      MLEmpty {} ->
        return Nothing

      _ ->
        error $ "lookup: `goto " ++ show k ++ "` ended in non-terminal node"

