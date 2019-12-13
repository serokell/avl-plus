-- | Proof-builder. Generates subtree with _uninteresting_ siblings replaced
--   with their respective hashes.
--
--   The operation produced the 'Proof' can still be run on this subtree,
--   no storage is needed.

module Data.Tree.AVL.Prune
    ( -- * Pruning
      prune
    ) where

import Control.Monad.Free (Free (Free))
import Data.Set (Set)
import qualified Data.Set as Set (notMember)
import Control.Lens ((&), (.~), (<&>), (^.))

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof

import Debug.Trace


-- | Proof baking: Cut off subtrees that haven't been touched.
prune ::
       forall h k v m. Retrieves h k v m
    => Set h
    -> Map h k v
    -> m (Proof h k v)
prune hashes tree = do
    traceShowM hashes
    Proof <$> go tree
  where
    go :: Map h k v -> m (Map h k v)
    go bush = do
        layer <- load bush
        if (layer^.mlHash) `Set.notMember` hashes
        then return $ Free (layer <&> ref . rootHash)
        else case layer of
            MLBranch {_mlLeft = l, _mlRight = r} -> do
                l' <- go l
                r' <- go r
                return $ Free $ layer
                    & mlLeft  .~ l'
                    & mlRight .~ r'

            _other ->
                return bush
