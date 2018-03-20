
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE LambdaCase         #-}

module Data.Tree.AVL.Prune where

import Control.Lens ((.~), (&))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data.Set (Set)
import Data.Hashable (Hashable)

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof
import Data.Tree.AVL.HashMapStore

import qualified Data.Set as Set

-- | Prune all subtrees that haven't been touched.
prune :: forall h k v m . (Ord h, Hashable h, MonadIO m, Stores h k v m) => Set Revision -> Map h k v -> m (Proof h k v)
prune revs tree = do
    start <- rootHash tree
    ((), db) <- runEmptyCache $ do
        whole <- go tree
        lift $ save whole
        return ()
    return $ Proof db start
  where
    go :: Map h k v -> HashMapStore h k v m (Map h k v)
    go bush = do
        rev  <- lift $ revision bush
        if Set.notMember rev revs
        then do
            pruned bush

        else do
            lift (open bush) >>= \case
              layer @ MLBranch {_mlLeft, _mlRight} -> do
                left  <- go _mlLeft
                right <- go _mlRight
                return $ close $ layer
                  & mlLeft  .~ left
                  & mlRight .~ right
              _other ->
                return bush