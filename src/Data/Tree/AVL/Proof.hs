
{-# language NamedFieldPuns #-}
{-# language TemplateHaskell #-}

module Data.Tree.AVL.Proof where

import Control.Lens ((&), (^.), (%~), makePrisms)

import Data.Tree.AVL.Internal

newtype Proof h k v = Proof { getProof :: Map h k v }
    deriving (Eq, Show)

makePrisms ''Proof

checkProof :: Hash h k v => h -> Proof h k v -> Bool
checkProof ideal (Proof proof) = go proof^.rootHash == ideal
  where
    go tree = case tree of
      Leaf   {} -> rehash tree
      Branch {} -> tree
        & setLeft  %~ go
        & setRight %~ go
        & rehash
      _other    -> tree

