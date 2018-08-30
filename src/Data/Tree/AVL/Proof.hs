-- | 'Proof' datatype and its checker.

module Data.Tree.AVL.Proof
    ( -- * Proof
      Proof (..)

      -- * Checker
    , checkProof
    )
  where

import Control.Monad.Free (iter)

import GHC.Generics (Generic)

import Data.Tree.AVL.Internal

-- | Proof for operation.
newtype Proof h k v = Proof
    { unProof :: Map h k v  -- ^ Get tree from a proof
    }
    deriving (Eq, Show, Generic)

-- | Check that rehashed proof's root hash is the same as given.
checkProof :: forall h k v . (Eq h, Hash h k v) => h -> Proof h k v -> Bool
checkProof ideal (Proof subtree) =
    iter hashOf' subtree == ideal
