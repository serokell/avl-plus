
{-# language NamedFieldPuns #-}

module Data.Tree.AVL.Insertion where

import Control.Lens hiding (locus, Empty)
import Control.Monad.State.Strict

import Data.List (foldl')
import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof
import Data.Tree.AVL.Zipper

insert :: Hash h k v => k -> v -> Map h k v -> (Proof h k v, Map h k v)
insert k v = runZipped (insert' True k v) UpdateMode

insertWithNoProof
    :: Hash h k v
    => k
    -> v
    -> Map h k v
    -> Map h k v
insertWithNoProof k v = snd . runZipped (insert' False k v) UpdateMode

insert' :: Hash h k v => Bool -> k -> v -> Zipped h k v (Proof h k v)
insert' needProof k v = do
    goto k
    rev <- use (locus.revision)
    proof <-
        if needProof
        then separately trackProof
        else return TrustMe

    tree <- use locus
    case tree of
      Empty {} -> do
        leaf <- makeLeaf k v minBound maxBound
        replaceWith leaf

      Leaf {} -> do
        let key  = tree^?!aptKey
            prev = tree^?!aptPrevKey
            next = tree^?!aptNextKey

        if k == key then do
            change $ do
                locus.aptValue .= v
        else do
            if k `inside` (prev, key)
            then do
                leaf <- makeLeaf k v prev key

                splitInsertBefore leaf
                unless (prev == minBound) $ do
                    goto prev
                    change $ do
                        locus.aptNextKey .= k

            else do
                leaf <- makeLeaf k v key next

                splitInsertAfter leaf
                unless (next == maxBound) $ do
                    goto next
                    change $ do
                        locus.aptPrevKey .= k
    return proof
  where
    splitInsertBefore leaf = do
        tree <- use locus
        rev  <- newRevision
        replaceWith (branch rev M leaf tree)
        descentRight
        change $ do
            locus.aptPrevKey .= k


    splitInsertAfter leaf = do
        tree <- use locus
        rev  <- newRevision
        replaceWith (branch rev M tree leaf)
        descentLeft
        change $ do
            locus.aptNextKey .= k

    makeLeaf k v prev next = do
        rev <- newRevision
        return Leaf
            { _aptRevision = rev
            , _aptValue    = v
            , _aptKey      = k
            , _aptHash     = hashOf (k, v, prev, next)
            , _aptNextKey  = next
            , _aptPrevKey  = prev
            }

    inside k (l, h) = k >= l && k <= h

fromList :: Hash h k v
    => [(k, v)]
    -> Map h k v
-- | Monomorphised version.
fromList = fromFoldable

fromFoldable :: Hash h k v => Foldable f => f (k, v) -> Map h k v
-- | Construct a tree from any Foldable (and calculate all hashes).
fromFoldable = foldl' (flip $ uncurry insertWithNoProof) empty
