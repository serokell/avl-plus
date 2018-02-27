
{-# language NamedFieldPuns             #-}
{-# language TemplateHaskell            #-}
{-# language DeriveGeneric              #-}
{-# language GeneralizedNewtypeDeriving #-}

module Data.Tree.AVL.Proof where

import Control.Lens ((&), (^.), (%~), makePrisms)

import Data.Binary

import GHC.Generics

import Data.Tree.AVL.Internal

newtype Proof h k v = Proof { getProof :: Map h k v }
    deriving (Eq, Show, Generic, Binary)

makePrisms ''Proof

checkProof :: Hash h k v => h -> Proof h k v -> Bool
checkProof ideal (Proof proof) = fullRehash proof^.rootHash == ideal

-- | Apply 'rehash' recursively.
fullRehash :: Hash h k v => Map h k v -> Map h k v
fullRehash tree = case tree of
  Empty  {} -> rehash tree
  Leaf   {} -> rehash tree
  Branch {} -> tree
    & setLeft  %~ fullRehash
    & setRight %~ fullRehash
    & rehash
  _other    -> tree

