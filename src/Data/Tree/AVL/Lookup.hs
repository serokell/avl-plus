
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Tree.AVL.Lookup where

import Control.Lens (to, use, (^.))

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof
import Data.Tree.AVL.Zipper

lookup' :: Stores h k v m => k -> Map h k v m -> m ((v, RevSet), Map h k v m)
lookup' k tree0 = do
    (mv, tree, trails) <- runZipped' (lookupZ k) UpdateMode tree0
    return ((mv, trails), tree)

lookup :: Stores h k v m => k -> Map h k v m -> m ((v, Proof h k v), Map h k v m)
lookup k tree0 = do
    (mv, tree, proof) <- runZipped (lookupZ k) UpdateMode tree0
    return ((mv, proof), tree)

lookupZ :: Hash h k v => k -> Zipped h k v m (Maybe v)
lookupZ k = do
    goto k
    tree <- use locus
    mark
    if
      | Just end <- tree^.terminal ->
        if end^.key == k
        then return (end^.value.to Just)
        else return Nothing

      | Just Vacuous <- tree^.vacuous ->
        return Nothing

      | otherwise ->
        error $ "lookup: `goto " ++ show k ++ "` ended in non-terminal node"

