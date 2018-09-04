-- | Proof-builder. Generates subtree with _uninteresting_ siblings replaced
--   with their respective hashes.
--
--   The operation produced the 'Proof' can still be run on this subtree,
--   no storage is needed.

module Data.Tree.AVL.Prune
    ( -- * Pruning
      prune
    )
  where

import Control.Lens ((&), (<&>), (.~), (^.))
import Control.Monad.Free (Free(Free))

import Data.Set (Set)

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof

import qualified Data.Set as Set (notMember)

-- | Prune all subtrees that haven't been touched.
prune :: forall h k v m . Retrieves h k v m => Set Revision -> FreshlyRehashed h k v -> m (Proof h k v)
prune hashes (getFreshlyRehashed -> tree) = do
    Proof <$> go tree
  where
    go :: Map h k v -> m (Map h k v)
    go bush = do
        layer <- load bush
        if Set.notMember (layer^.mlRevision) hashes
        then do
            return $ Free (layer <&> ref . unsafeRootHash)

        else do
            case layer of
              MLBranch {_mlLeft = l, _mlRight = r} -> do
                l' <- go l
                r' <- go r
                return $ Free $ layer
                    & mlLeft  .~ l'
                    & mlRight .~ r'

              _other ->
                return bush
