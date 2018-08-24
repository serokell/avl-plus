module Data.Tree.AVL.Proof where

import Control.Lens (makePrisms)
import Control.Monad.Free (iter)

import GHC.Generics (Generic)

import Data.Tree.AVL.Internal

newtype Proof h k v = Proof { unProof :: Map h k v }
    deriving (Eq, Show, Generic)

makePrisms ''Proof

checkProof :: forall h k v . (Eq h, Hash h k v) => h -> Proof h k v -> Bool
checkProof ideal (Proof subtree) =
    iter hashOf' subtree == ideal
