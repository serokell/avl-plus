
{-# LANGUAGE MultiWayIf     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Tree.AVL.Deletion (delete, deleteWithNoProof, delete') where

import Control.Lens (use, (%=))
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof
import Data.Tree.AVL.Zipper

-- | Endpoint that allows to merge proofs for some sequental operations.
delete' :: Stores h k v m => k -> Map h k v -> m (RevSet, Map h k v)
delete' k tree = do
    (_yes, res, trails) <- runZipped' (deleteZ k) DeleteMode tree
    return (trails, res)

-- | Endpoint that generates proof.
delete :: Stores h k v m => k -> Map h k v -> m (Proof h k v, Map h k v )
delete k tree = do
    (_yes, res, proof) <- runZipped (deleteZ k) DeleteMode tree
    return (proof, res)

-- | Endpoint that generates no proof.
deleteWithNoProof
    :: Stores h k v m
    => k
    -> Map h k v m
    -> m (Map h k v m)
deleteWithNoProof k tree = do
    (_yes, res, _proof) <- runZipped (deleteZ k) DeleteMode tree
    return res

-- | Deletion algorithm.
deleteZ :: Stores h k v m => k -> Zipped h k v m Bool
deleteZ k = do
    tree  <- use locus
    layer <- lift $ pick tree
    case layer of
      MLLeaf { _mlKey } -> do
        if _mlKey == k
        then do
            replaceWith empty
            return True

        else do
            mark
            return False

      MLEmpty {} -> do
        mark
        return False

      _ -> do
        goto k
        tree0  <- use locus
        layer0 <- lift $ pick tree0
        case layer0 of
          MLLeaf { _mlKey = key0, _mlPrevKey = prev, _mlNextKey = next } -> do
            if key0 /= k
            then do
                return False

            else do
                side <- up  -- return to a parent of node to be deleted

                -- we need to mark another child, so it ends in a proof
                _ <- case side of
                  L -> descentRight >> mark >> up
                  R ->  descentLeft >> mark >> up

                here      <- use locus
                hereLayer <- lift $ pick here
                let
                  newTree = case hereLayer of
                    MLBranch { _mlLeft = left, _mlRight = right } ->
                      case side of
                        L -> right
                        R -> left

                    _ ->
                        error "delete: successful `up` ended in non-Branch"

                replaceWith newTree  -- replace with another child

                unless (prev == minBound) $ do
                    goto prev
                    change (locus %= setNextKey next)

                unless (next == maxBound) $ do
                    goto next
                    change (locus %= setPrevKey prev)

                return True

          _ -> do
            error $ "insert: `goto " ++ show k ++ "` ended in non-terminal node"

