
{-# language NamedFieldPuns #-}

module Data.Tree.AVL.Deletion where

import Control.Lens hiding (locus, Empty)
import Control.Monad.State.Strict

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof
import Data.Tree.AVL.Zipper

delete :: Hash h k v => k -> Map h k v -> ((Bool, Proof h k v), Map h k v)
delete k tree = ((yes, proof), res)
  where
    (yes, res, proof) = runZipped (delete' k) DeleteMode tree

deleteWithNoProof
    :: Hash h k v
    => k
    -> Map h k v
    -> Map h k v
deleteWithNoProof k tree = res
  where
    (_yes, res, _proof) = runZipped (delete' k) DeleteMode tree

delete' :: Hash h k v => k -> Zipped h k v Bool
delete' k = do
    tree <- use locus
    case tree of
      leaf0 | Just term <- leaf0^.terminal -> do
        if term^.key == k
        then do
            replaceWith empty
            return True

        else do
            change $ return ()
            return False

      Empty {} -> do
        return False

      _ -> do
        goto k
        tree0 <- use locus
        case tree0 of
          leaf1 | Just term <- leaf1^.terminal -> do
            let key0 = term^.key
                prev = term^.prevKey
                next = term^.nextKey

            if key0 /= k
            then do
                return False

            else do
                side <- up
                here <- use locus
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

          other -> do
            error $ "insert: `goto k` ended in non-terminal node - " ++ show other

