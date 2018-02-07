
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

pruneNode :: Hash h k v => Map h k v -> Map h k v
pruneNode tree = case tree of
  Pruned {} -> tree
  Branch {} -> pruned (tree^.revision) (tree^?!aptPayload)
  Leaf   {} -> pruned (tree^.revision) Payload
    { _pRootHash  = tree^.rootHash
    , _pMinKey    = tree^?!aptKey
    , _pCenterKey = tree^?!aptKey
    }

  Empty {} -> pruned (tree^.revision) Payload
    { _pRootHash  = emptyOne
    , _pMinKey    = minBound
    , _pCenterKey = minBound
    }

