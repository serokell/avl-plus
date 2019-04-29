module Insertion (tests) where

import Common

import qualified Data.Tree.AVL as AVL
import qualified Data.Tree.AVL.Internal as Internal

tests :: Spec
tests = describe "Insert" $ do
    it' "toList . fromList == sort . unique" $ \list -> do
        tree <- AVL.fromList list
        back <- AVL.toList tree
        let uniq = uniqued list
        return (back == uniq)

    it' "Tree is as balanced as possible" $ \list -> do
        tree <- AVL.fromList list
        Internal.isBalancedToTheLeaves tree

    describe "Proofs" $ do
        it' "Insert proof is verifiable" $ \(k, v, list) -> do
            tree      <- AVL.fromList list
            (set0, _) <- AVL.insert k v tree
            (set1, _) <- AVL.insert k v . AVL.unProof =<< AVL.prune set0 tree
            AVL.checkProof (AVL.rootHash tree) <$> AVL.prune set1 tree

        it' "Insert proof is replayable" $ \(k, v, list) -> do
            tree          <- AVL.fromList list
            (set0, tree1) <- AVL.insert k v tree
            (set1, tree2) <- AVL.insert k v . AVL.unProof =<< AVL.prune set0 tree
            return (set0 == set1 && tree1 == tree2)

        it' "Insert is idempotent" $ \(k, v, list) -> do
            tree  <- AVL.fromList list
            tree1 <- AVL.insertWithNoProof k v tree
            tree2 <- AVL.insertWithNoProof k v tree1
            return (tree1 == tree2)
