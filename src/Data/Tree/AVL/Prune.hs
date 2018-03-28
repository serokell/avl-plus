
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE LambdaCase         #-}

module Data.Tree.AVL.Prune where

import Control.Lens ((.~), (&))
import Control.Monad.Free
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data.Set (Set)
import Data.Hashable (Hashable)

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof
import Data.Tree.AVL.HashMapStore

import qualified Data.Set as Set

-- | Prune all subtrees that haven't been touched.
prune :: forall h k v m . (Ord h, Hashable h, MonadIO m, Stores h k v m) => Set h -> Map h k v -> m (Proof h k v)
prune hashes tree = do
    pruned <- go tree
    return $ Proof pruned
  where
    go :: Map h k v -> m (Map h k v)
    go bush = do
        let hash = rootHash bush
        case bush of
          Pure h     -> return $ Pure h
          Free layer -> do
            saveOne bush
            if Set.notMember hash hashes
            then do
                isolate bush

            else do
                case layer of
                  MLBranch {_mlTilt = t, _mlLeft = l, _mlRight = r} -> do
                    l' <- go l
                    r' <- go r
                    branch t l' r'

                  _other ->
                    return bush