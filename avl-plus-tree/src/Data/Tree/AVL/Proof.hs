
{-# language NamedFieldPuns #-}
{-# language TemplateHaskell #-}

module Data.Tree.AVL.Proof where

-- import Control.Applicative
import Control.Lens hiding (locus, Empty)
-- import Control.Monad.State.Strict

-- import Debug.Trace as Debug

import Data.Tree.AVL.Internal
-- import Data.Tree.AVL.Zipper
-- import Data.Tree.AVL.Prune

newtype Proof h k v = Proof { getProof :: Map h k v } deriving Show

checkProof :: Hash h k v => h -> Proof h k v -> Bool
checkProof ideal (Proof proof) = go proof^.rootHash == ideal
  where
    go tree = case tree of
      Leaf   {} -> rehash tree
      Branch {} -> tree
        & _Fix.mlLeft  %~ go
        & _Fix.mlRight %~ go
        & rehash
      _other    -> tree

