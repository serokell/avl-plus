
{-# language FlexibleContexts #-}

module Data.Tree.AVL.Prune where

import Control.Lens hiding (Empty)

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof

prune :: Hash h k v => Revision -> Map h k v -> Proof h k v
prune rev = Proof . go
  where
    go tree
      | tree^.revision <= rev =
        pruned tree

    go tree = case tree of
      Branch {} -> tree
        & _Fix.mlLeft  %~ go
        & _Fix.mlRight %~ go

      other ->
        other

