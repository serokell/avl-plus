{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Tree.AVL.Proof where

import           Control.Lens           (makePrisms)
import           Control.Monad.Free     (iter)

import           GHC.Generics           (Generic)

import           Data.Tree.AVL.Internal

data Proof h k v = Proof { subtree :: Map h k v }
    deriving (Show, Generic)

makePrisms ''Proof

checkProof :: forall h k v . Hash h k v => h -> Proof h k v -> Bool
checkProof ideal (Proof subtree) =
    iter hashOf' subtree == ideal
