
{-# language NamedFieldPuns #-}
{-# language TemplateHaskell #-}

module Data.Tree.AVL.Proof where

import Control.Lens ((&), (^.), (%~), makePrisms)

import Data.Tree.AVL.Internal

newtype Proof h k v = Proof { getProof :: Map h k v }
    deriving (Eq, Show)

makePrisms ''Proof

checkProof :: Hash h k v => h -> Proof h k v -> Bool
checkProof ideal (Proof proof) = fullRehash proof^.rootHash == ideal

fullRehash :: Hash h k v => Map h k v -> Map h k v
fullRehash tree = case tree of
  Empty  {} -> rehash tree
  Leaf   {} -> rehash tree
  Branch {} -> tree
    & setLeft  %~ fullRehash
    & setRight %~ fullRehash
    & rehash
  _other    -> tree

