module Insertion (tests) where

import Common

import qualified Data.Tree.AVL as AVL
import qualified Data.Tree.AVL.Insertion as AVL
import qualified Data.Tree.AVL.Internal as AVL

tests :: Spec
tests = describe "Insert" $ do
    it' "tfsu toList . fromList == sort . unique" $ \() -> do
        let list@ [m, i, d] = [StringName "M", StringName "I", StringName "D"]
        let tree0 = AVL.empty :: M
        tree1 <- AVL.insertWithNoProof m 0 tree0
        tree2 <- AVL.insertWithNoProof i 0 tree1
        tree3 <- AVL.insertWithNoProof d 0 tree2
        back <- AVL.toList tree3
        let uniq = uniqued $ map (, 0) list
        return (back == uniq)
            :: StorageMonad Bool

    it' "Tree is as balanced as possible" $ \list -> do
        tree <- AVL.fromList list :: StorageMonad M
        AVL.isBalancedToTheLeaves tree

    describe "Proofs" $ do
        it' "Insert proof is verifiable" $ \(k, v, list) -> do
            tree        <- AVL.fromList list :: StorageMonad M
            (proof,  _) <- AVL.insert' k v tree
            (proof1, _) <- AVL.insert' k v (AVL.unProof proof)

            return $ AVL.checkProof (AVL.rootHash tree) proof1

        it' "Insert proof is replayable" $ \(k, v, list) -> do
            tree        <- AVL.fromList list :: StorageMonad M
            (proof1, _) <- AVL.insert' k v tree
            (proof2, _) <- AVL.insert' k v (AVL.unProof proof1)

            return (proof1 == proof2)

        -- -- It is not, unless we give `Eq v` constraint to the `insertZ`.
        --
        -- it' "Insert is idempotent" $ \(k, v, list) -> do
        --     tree <- AVL.fromList list :: StorageMonad M
        --     (_, tree1) <- AVL.insert' k v tree
        --     (_, tree2) <- AVL.insert' k v tree1
        --     return (tree1 == tree2)
