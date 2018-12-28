module Deletion (tests) where

import Data.Foldable (for_)
import Data.Traversable (for)

import Data.List ((\\), nub)

import Common

import qualified Data.Tree.AVL as AVL
import qualified Data.Tree.AVL.Deletion as AVL
import qualified Data.Tree.AVL.Internal as AVL

tests :: Spec
tests = describe "Delete" $ do
    it' "Tree is still balanced after delete" $ \list -> do
        tree  <- AVL.fromList list :: StorageMonad M
        trees <- scanM (AVL.deleteWithNoProof . fst) tree list
        yes   <- and <$> for trees AVL.isBalancedToTheLeaves

        for_ trees $ AVL.isBalancedToTheLeaves

        return yes

    it' "Deletion deletes" $ \(k, v, list) -> do
        tree  <- AVL.fromList ((k, v) : list) :: StorageMonad M
        tree1 <- AVL.deleteWithNoProof k tree
        list' <- AVL.toList tree1
        let diff = nub (map fst list) \\ map fst list'
        return
            $  diff == [k] && k `elem`    map fst list
            || diff == []  && k `notElem` map fst list

    describe "Proofs" $ do
        it' "Delete proof is verifiable" $ \(k, v, list) -> do
            tree        <- AVL.fromList ((k, v) : list) :: StorageMonad M
            (proof,  _) <- AVL.delete' k tree
            (proof1, _) <- AVL.delete' k (AVL.unProof proof)
            return $ AVL.checkProof (AVL.rootHash tree) proof1

        it' "Delete proof is replayable" $ \(k, v, list) -> do
            tree        <- AVL.fromList ((k, v) : list) :: StorageMonad M
            (proof1, _) <- AVL.delete k tree
            (proof2, _) <- AVL.delete k . AVL.unProof =<< AVL.prune proof1 tree
            return (proof1 == proof2)
                :: StorageMonad Bool

        it' "Delete proof is verifiable (even if there's nothing to delete)" $ \(k, list) -> do
            tree        <- AVL.fromList list :: StorageMonad M
            (proof,  _) <- AVL.delete' k tree
            (proof1, _) <- AVL.delete' k (AVL.unProof proof)
            return $ AVL.checkProof (AVL.rootHash tree) proof1

        it' "Delete is idempotent" $ \(k, list) -> do
            tree       <- AVL.fromList list :: StorageMonad M
            (_, tree1) <- AVL.delete k tree
            (_, tree2) <- AVL.delete k tree1
            return (tree1 == tree2)
