
{-# language NamedFieldPuns #-}

module Data.Tree.AVL.Insertion where

import Control.Lens hiding (locus, Empty)
import Control.Monad.State.Strict

import Data.List (foldl')
import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof
import Data.Tree.AVL.Zipper

insert :: Hash h k v => k -> v -> Map h k v -> (Proof h k v, Map h k v)
insert k v tree = (proof, res)
  where
    ((), res, proof) = runZipped (insert' k v) UpdateMode tree

insertWithNoProof
    :: Hash h k v
    => k
    -> v
    -> Map h k v
    -> Map h k v
insertWithNoProof k v tree = res
  where
    ((), res, _) = runZipped (insert' k v) UpdateMode tree

insert' :: Hash h k v => k -> v -> Zipped h k v ()
insert' k v = do
    goto k
    tree <- use locus
    case tree of
      Empty {} -> do
        leaf0 <- makeLeaf k v minBound maxBound
        replaceWith leaf0

      leaf1 | Just term <- leaf1^.terminal -> do
        let key0 = term^.key
            prev = term^.prevKey
            next = term^.nextKey

        if k == key0 then do
            change $ do
                locus.setValue .= v
        else do
            if k `isInside` (prev, key0)
            then do
                leaf0 <- makeLeaf k v prev key0

                splitInsertBefore leaf0
                unless (prev == minBound) $ do
                    goto prev
                    change $ do
                        locus.setNextKey .= k

            else do
                leaf0 <- makeLeaf k v key0 next

                splitInsertAfter leaf0
                unless (next == maxBound) $ do
                    goto next
                    change $ do
                        locus.setPrevKey .= k
      other -> do
        error $ "insert: `goto k` ended in non-terminal node - " ++ show other

    return ()
  where
    splitInsertBefore leaf0 = do
        tree <- use locus
        rev  <- newRevision
        replaceWith (branch rev M leaf0 tree)
        descentRight
        change $ do
            locus.setPrevKey .= k


    splitInsertAfter leaf0 = do
        tree <- use locus
        rev  <- newRevision
        replaceWith (branch rev M tree leaf0)
        descentLeft
        change $ do
            locus.setNextKey .= k

    makeLeaf k0 v0 prev next = do
        rev <- newRevision
        return $ leaf rev k0 v0 prev next

    isInside k0 (l, h) = k0 >= l && k0 <= h

fromList :: Hash h k v
    => [(k, v)]
    -> Map h k v
-- | Monomorphised version.
fromList = fromFoldable

fromFoldable :: Hash h k v => Foldable f => f (k, v) -> Map h k v
-- | Construct a tree from any Foldable (and calculate all hashes).
fromFoldable = foldl' (flip $ uncurry insertWithNoProof) empty
