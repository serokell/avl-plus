
{-# language NamedFieldPuns #-}
{-# language MultiWayIf     #-}

module Data.Tree.AVL.Deletion (delete, deleteWithNoProof, delete') where

import Control.Lens  ((^.), (.=), use)
import Control.Monad (unless)

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof
import Data.Tree.AVL.Zipper

delete' :: Hash h k v => k -> Map h k v -> (RevSet, Map h k v)
delete' k tree = (trails, res)
  where
    (_yes, res, trails) = runZipped' (deleteZ k) DeleteMode tree

delete :: Hash h k v => k -> Map h k v -> (Proof h k v, Map h k v)
delete k tree = (proof, res)
  where
    (_yes, res, proof) = runZipped (deleteZ k) DeleteMode tree

deleteWithNoProof
    :: Hash h k v
    => k
    -> Map h k v
    -> Map h k v
deleteWithNoProof k tree = res
  where
    (_yes, res, _proof) = runZipped (deleteZ k) DeleteMode tree

deleteZ :: Hash h k v => k -> Zipped h k v Bool
deleteZ k = do
    tree <- use locus
    if
      | Just term <- tree^.terminal -> do
        if term^.key == k
        then do
            replaceWith empty
            return True

        else do
            mark
            return False

      | Just Vacuous <- tree^.vacuous -> do
        mark
        return False

      | otherwise -> do
        goto k
        tree0 <- use locus
        if
          | Just term <- tree0^.terminal -> do
            let key0 = term^.key
                prev = term^.prevKey
                next = term^.nextKey

            if key0 /= k
            then do
                return False

            else do
                side <- up
                _ <- case side of
                  L -> descentRight >> mark >> up
                  R ->  descentLeft >> mark >> up

                here <- use locus
                mark
                let
                  newTree
                    | Just fork <- here^.branching =
                      case side of
                        L -> fork^.right
                        R -> fork^.left

                    | otherwise =
                        error "delete: successful `up` ended in non-Branch"

                replaceWith newTree

                unless (prev == minBound) $ do
                    goto prev
                    change (locus.setNextKey .= next)

                unless (next == maxBound) $ do
                    goto next
                    change (locus.setPrevKey .= prev)

                return True

          | otherwise -> do
            error $ "insert: `goto k` ended in non-terminal node - " ++ show tree0

