module Data.Tree.AVL.Prune where

import Control.Lens ((&), (<&>), (.~), (^.))
import Control.Monad.Free     (Free(Free, Pure))

import Data.Set               (Set)

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof

import qualified Data.Set as Set (notMember)

-- | Prune all subtrees that haven't been touched.
prune :: forall h k v . Hash h k v => Set Revision -> FreshlyRehashed h k v -> Proof h k v
prune hashes (FreshlyRehashed tree) = do
    let pruned = go tree
    Proof pruned
  where
    go :: Map h k v -> Map h k v
    go bush = do
        case bush of
          Pure h     -> Pure h
          Free layer -> do
            if Set.notMember (layer^.mlRevision) hashes
            then do
                Free $ layer <&> \point ->
                    case rootHash point of
                      Nothing -> error
                        $ "prune: fullRehash did not rehash node, rev: "
                        ++ show (layer^.mlRevision)
                      Just hash ->
                        ref hash
            else do
                case layer of
                  MLBranch {_mlLeft = l, _mlRight = r} -> do
                    let l' = go l
                    let r' = go r
                    Free $ layer
                        & mlLeft  .~ l'
                        & mlRight .~ r'

                  _other ->
                    bush
