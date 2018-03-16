
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE ScopedTypeVariables         #-}

module Data.Tree.AVL.Prune where

import Control.Lens ((.~), (&))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data.Set (Set)

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof
import Data.Tree.AVL.HashMapStore

import qualified Data.Set as Set

-- | Prune all subtrees that haven't been touched.
prune :: forall h k v m . (Ord h, MonadIO m, Stores h k v m) => Set Revision -> Map h k v m -> m (Proof h k v)
prune revs tree = do
    start <- rootHash tree
    ((), db) <- runEmptyCache $ do
        whole <- go start
        lift $ save whole
        return ()
    return $ Proof db start
  where
    go :: h -> HashMapStore h k v m (Map h k v m)
    go hash = do
        bush <- lift $ pickTree hash
        rev  <- lift $ revision tree
        if Set.notMember rev revs
        then do
            return $ pruned bush

        else do
            layer <- lift $ pick bush
            case layer of
              MLBranch {_mlLeft, _mlRight} -> do
                left  <- (lift . rootHash) =<< go _mlLeft
                right <- (lift . rootHash) =<< go _mlRight
                return $ hide $ layer
                  & mlLeft  .~ left
                  & mlRight .~ right
              _other ->
                return bush