
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Data.Tree.AVL.Proof where

import Control.Lens (makePrisms, (.~), (&), (^.))
import Control.Monad.Trans.Free
import Control.Monad.Trans.Class

import Data.Binary
import Data.Hashable
import Data.Fix (Fix (..))
import Data.HashMap.Strict as HM

import GHC.Generics

import Data.Tree.AVL.Internal
import Data.Tree.AVL.HashMapStore
import Data.Tree.AVL.KVStoreMonad

data Proof   h k v   = Proof { database :: HashMap h (MapLayer h k v h), root :: h }
    deriving (Generic, Binary)

instance  (Hashable k, Eq k, Binary k, Binary v) => Binary (HM.HashMap k v) where
  get = HM.fromList <$> get
  put = put . HM.toList

deriving instance Binary (f (Fix f)) => Binary (Fix f)

makePrisms ''Proof

checkProof :: forall h k v m . Stores h k v m => h -> Proof h k v -> m Bool
checkProof ideal (Proof db root) = do
    seed db
    let rehashed = fullRehash root :: Map h k v m
    hash <- rootHash rehashed :: m h
    return $ hash == ideal
  where
    -- | Apply 'rehash' recursively.
    fullRehash :: h -> Map h k v m
    fullRehash root = do
        tree <- lift $ pickTree root
        layer <- lift $ pick tree
        case layer of
          MLEmpty  {} -> rehash tree
          MLLeaf   {} -> rehash tree
          MLBranch {_mlLeft, _mlRight} -> do
            newLeft  <- lift $ rootHash (fullRehash _mlLeft)
            newRight <- lift $ rootHash (fullRehash _mlRight)
            hide $ layer
              & mlLeft  .~ newLeft
              & mlRight .~ newRight
          _other    -> tree

