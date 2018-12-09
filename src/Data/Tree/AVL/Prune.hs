-- | Proof-builder. Generates subtree with _uninteresting_ siblings replaced
--   with their respective hashes.
--
--   The operation produced the 'Proof' can still be run on this subtree,
--   no storage is needed.

module Data.Tree.AVL.Prune
    ( -- * Pruning
      prune
    ) where

import Lens.Micro.Platform ((&), (.~), (<&>), (^.))
import Control.Monad.Free (Free (Free))
import Data.Set (Set)
import qualified Data.Set as Set (notMember)

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof


-- | Proof baking: Cut off subtrees that haven't been touched.
prune ::
       forall h k v m. Retrieves h k v m
    => Set h
    -> Map h k v
    -> m (Proof h k v)
prune hashes (fullRehash -> tree) =
    Proof <$> go tree
  where
    go :: Map h k v -> m (Map h k v)
    go bush = do
        layer <- load bush
        if maybe False (`Set.notMember` hashes) (layer^.mlHash)
        then return $ Free (layer <&> ref . unsafeRootHash)
        else case layer of
            MLBranch {_mlLeft = l, _mlRight = r} -> do
                l' <- go l
                r' <- go r
                return $ Free $ layer
                    & mlLeft  .~ l'
                    & mlRight .~ r'
            _other -> return bush
