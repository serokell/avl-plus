
{-# language NamedFieldPuns #-}

module Data.Tree.AVL.Deletion where

import Control.Lens hiding (locus, Empty)
import Control.Monad.State.Strict

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof
import Data.Tree.AVL.Zipper

delete :: Hash h k v => k -> Map h k v -> ((Bool, Proof h k v), Map h k v)
delete k = runZipped (delete' True k) DeleteMode

deleteWithNoProof
    :: Hash h k v
    => k
    -> Map h k v
    -> Map h k v
deleteWithNoProof k = snd . runZipped (delete' False k) DeleteMode

delete' :: Hash h k v => Bool -> k -> Zipped h k v (Bool, Proof h k v)
delete' needProof k = do
    tree <- use locus
    case tree of
      Leaf {} -> do
        if tree^?!aptKey == k
        then do
            datum <- leafDataForProof
            replaceWith empty
            return (True, Proof [] datum)

        else do
            datum <- leafDataForProof
            return (False, Proof [] datum)

      Empty {} -> do
        return (False, TreeWasEmpty)

      _ -> do
        goto k
        proof <-
            if needProof
            then separately trackProof
            else return TrustMe

        tree <- use locus
        case tree of
          Leaf {} -> do
            let key  = tree^?!aptKey
                prev = tree^?!aptPrevKey
                next = tree^?!aptNextKey

            if key /= k
            then do
                return (False, proof)

            else do
                side    <- up
                Just newTree <- case side of
                  L -> preuse (locus.aptRight)
                  R -> preuse (locus.aptLeft)

                replaceWith newTree

                unless (prev == minBound) $ do
                    goto prev
                    change (locus.aptNextKey .= next)

                unless (next == maxBound) $ do
                    goto next
                    change (locus.aptPrevKey .= prev)

                return (True, proof)
