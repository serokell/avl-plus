
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
import Control.Monad.Catch
import Control.Monad.Free
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class

import Data.Binary
import Data.Hashable
import Data.Fix (Fix (..))
import Data.HashMap.Strict as HM

import GHC.Generics

import Data.Tree.AVL.Internal
import Data.Tree.AVL.KVStoreMonad
import Data.Tree.AVL.HashMapStore

data Proof h k v = Proof { subtree :: Map h k v }
    deriving (Show, Generic, Binary)

instance (Hashable k, Eq k, Binary k, Binary v) => Binary (HM.HashMap k v) where
  get = HM.fromList <$> get
  put = put . HM.toList

deriving instance Binary (f (Fix f)) => Binary (Fix f)

makePrisms ''Proof

checkProof :: forall h k v m . Stores h k v m => h -> Proof h k v -> m Bool
checkProof ideal (Proof subtree) = do
    renewed <- fullRehash subtree
    return $ rootHash renewed == ideal
  where
    -- | Apply 'rehash' recursively.
    fullRehash :: Map h k v -> m (Map h k v)
    fullRehash tree = do
        open tree >>= \case
          layer @ MLBranch {_mlLeft, _mlRight} -> do
            left  <- fullRehash _mlLeft
            right <- fullRehash _mlRight
            rehash $ close $ layer
              & mlLeft  .~ left
              & mlRight .~ right

          _other -> do
            rehash tree
      `catch` \(NotFound (_ :: h)) -> do
        return tree
