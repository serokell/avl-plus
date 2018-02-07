
{-# language NamedFieldPuns #-}
{-# language TemplateHaskell #-}

module Data.Tree.AVL.Proof where

import Control.Applicative
import Control.Lens hiding (locus, Empty)
-- import Control.Monad.State.Strict

-- import Debug.Trace as Debug

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Zipper
import Data.Tree.AVL.Prune

newtype Proof h k v = Proof { getProof :: Map h k v }

checkProof :: Hash h k v => h -> Proof h k v
checkProof ideal = go . getProof
  where
    go tree = case rehash tree of
      Leaf {} ->
