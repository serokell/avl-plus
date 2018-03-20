
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Data.Tree.AVL.Proof where

import Control.Lens (makePrisms, (.~), (&))
import Control.Monad.Trans.Class

import Data.Binary
import Data.Hashable
import Data.Fix (Fix (..))
import Data.HashMap.Strict as HM

import GHC.Generics

import Data.Tree.AVL.Internal
import Data.Tree.AVL.KVStoreMonad

data Proof h k v = Proof { database :: HashMap h (MapLayer h k v h), root :: h }
    deriving (Generic, Binary)

instance (Hashable k, Eq k, Binary k, Binary v) => Binary (HM.HashMap k v) where
  get = HM.fromList <$> get
  put = put . HM.toList

deriving instance Binary (f (Fix f)) => Binary (Fix f)

makePrisms ''Proof

checkProof :: forall h k v m . Stores h k v m => h -> Proof h k v -> m Bool
checkProof ideal (Proof db root) = do
    seed db
    renewed <- fullRehash (ref root :: Map h k v)
    theHash <- rootHash renewed
    return $ theHash == ideal
  where
    -- | Apply 'rehash' recursively.
    fullRehash :: Map h k v -> m (Map h k v)
    fullRehash tree = do
        open tree >>= \case
          layer @ MLBranch {_mlLeft, _mlRight} -> do
            left  <- fullRehash _mlLeft
            right <- fullRehash _mlRight
            return $ close $ layer
              & mlLeft  .~ left
              & mlRight .~ right
          
          _other -> do
            rehash tree

