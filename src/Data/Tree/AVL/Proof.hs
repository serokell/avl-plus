
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}

module Data.Tree.AVL.Proof where

import Control.Lens               (makePrisms, (.~), (&))
import Control.Monad.Catch        (catch)

import GHC.Generics               (Generic)

import Data.Tree.AVL.Internal

data Proof h k v = Proof { subtree :: Map h k v }
    deriving (Show, Generic)

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
