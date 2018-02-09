
{-# language FlexibleContexts #-}

module Data.Tree.AVL.Prune where

import Control.Lens hiding (Empty)

import Data.Tree.AVL.Internal

prune :: Hash h k v => Revision -> Map h k v -> Map h k v
prune rev = go
  where
    go tree
      | tree^.revision <= rev =
        pruneNode tree

    go tree = case tree of
      Branch {} -> tree
        & aptLeft  %~ go
        & aptRight %~ go

      other ->
        other

