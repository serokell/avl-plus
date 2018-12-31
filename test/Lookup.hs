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
            tree           <- AVL.fromList list :: StorageMonad M
            ((_, set0), _) <- AVL.lookup k tree

            AVL.checkProof (AVL.rootHash tree) <$> AVL.prune set0 tree

        it' "Generated proofs are replayable" $ \(k, list) -> do
            tree              <- AVL.fromList list :: StorageMonad M
            ((res,  set0), _) <- AVL.lookup k tree
            ((res1, set1), _) <- AVL.lookup k . AVL.unProof =<< AVL.prune set0 tree

            return $ res   == res1
                  && set0  == set1
