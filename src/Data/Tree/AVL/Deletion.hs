
{-# LANGUAGE MultiWayIf     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Tree.AVL.Deletion (delete, deleteWithNoProof, delete') where

import Control.Lens (use, (.=))
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)

import Data.Set (Set)
import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof
import Data.Tree.AVL.Zipper

-- | Endpoint that allows to merge proofs for some sequental operations.
delete' :: Stores h k v m => k -> Map h k v -> m (Set h, Map h k v)
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
    -> Map h k v
    -> m (Map h k v)
deleteWithNoProof k tree = do
    (_yes, res, _proof) <- runZipped (deleteZ k) DeleteMode tree
    return res

-- | Deletion algorithm.
deleteZ :: forall h k v m . Stores h k v m => k -> Zipped h k v m Bool
deleteZ k = do
    withLocus $ \case
      MLLeaf { _mlKey } -> do
        if _mlKey == k
        then do
            e <- lift empty
            replaceWith (e :: Map h k v)
            return True

        else do
            mark "deleteZ/node exists"
            return False

      MLEmpty {} -> do
        --mark
        return False

      _ -> do
        goto k
        withLocus $ \case
          MLLeaf { _mlKey = key0, _mlPrevKey = prev, _mlNextKey = next } -> do
            if key0 /= k
            then do
                return False

            else do
                side <- up  -- return to a parent of node to be deleted

                -- we need to mark another child, so it ends in a proof
                _ <- case side of
                  L -> descentRight >> up
                  R ->  descentLeft >> up

                newTree <- withLocus $ \case
                  MLBranch { _mlLeft = left, _mlRight = right } ->
                    return $ case side of
                      L -> right
                      R -> left

                  _ ->
                      error "delete: successful `up` ended in non-Branch"

                replaceWith newTree  -- replace with another child

                unless (prev == minBound) $ do
                    goto prev
                    change $ do
                      loc   <- use locus
                      loc'  <- setNextKey next loc
                      locus .= loc'

                unless (next == maxBound) $ do
                    goto next
                    change $ do
                      loc   <- use locus
                      loc'  <- setPrevKey prev loc
                      locus .= loc'

                return True

          _ -> do
            error $ "insert: `goto " ++ show k ++ "` ended in non-terminal node"

