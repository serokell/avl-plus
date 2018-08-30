module Lookup (tests) where

import Common

import qualified Data.Tree.AVL as AVL

tests :: Spec
tests = describe "Lookup" $ do
    it' "Lookup actually works" $ \(list) -> do
        case uniqued list of
          [] -> do
            return True

          (k, v) : rest -> do
            tree              <- AVL.fromList ((k, v) : rest) :: StorageMonad M
            ((Just v1, _), _) <- AVL.lookup k tree

            return (v == v1)

    describe "Proofs" $ do
        it' "Generated proofs are verified" $ \(k, list) -> do
            tree            <- AVL.fromList list :: StorageMonad M
            ((_, proof), _) <- AVL.lookup k tree

            let Just hash = AVL.rootHash (AVL.assignHashes tree)

            return $ AVL.checkProof hash proof

        it' "Generated proofs are replayable" $ \(k, list) -> do
            tree              <- AVL.fromList list :: StorageMonad M
            ((res, proof), _) <- AVL.lookup k tree

            let Just hash = AVL.rootHash (AVL.assignHashes tree)
            let AVL.Proof subtree = proof

            ((res1, proof1), _) <- AVL.lookup k subtree

            return $ AVL.checkProof hash proof
                  && res   == res1
                  && proof == proof1
