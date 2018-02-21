
{-# language FlexibleContexts #-}

module Data.Tree.AVL.Prune where

import Control.Lens ((&), (^.), (%~))

import Data.Set     (Set)

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof

import qualified Data.Set as Set

prune :: Hash h k v => Set Revision -> Map h k v -> Proof h k v
prune revs = Proof . go
  where
    go tree
      | Set.notMember (tree^.revision) revs =
        pruned tree

    go tree = case tree of
      Branch {} -> tree
        & setLeft  %~ go
        & setRight %~ go

      other ->
        other

