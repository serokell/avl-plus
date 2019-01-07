module Deletion (tests) where

import Data.Traversable (for)

import Data.List ((\\), nub)

import Common

import qualified Data.Tree.AVL as AVL
import qualified Data.Tree.AVL.Internal as Internal

tests :: Spec
tests = describe "Delete" $ do
    it' "Tree is still balanced after delete" $ \list -> do
        tree  <- AVL.fromList list :: StorageMonad M
        trees <- scanM (AVL.deleteWithNoProof . fst) tree list
        yes   <- and <$> for trees Internal.isBalancedToTheLeaves
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
        it' "Delete proof is verifiable" $ \(k, list) -> do
            tree      <- AVL.fromList list :: StorageMonad M
            (set0, _) <- AVL.delete k tree
            (set1, _) <- AVL.delete k . AVL.unProof =<< AVL.prune set0 tree
            AVL.checkProof (AVL.rootHash tree) <$> AVL.prune set1 tree

        it' "Delete proof is replayable" $ \(k, list) -> do
            tree          <- AVL.fromList list :: StorageMonad M
            (set0, tree1) <- AVL.delete k tree
            (set1, tree2) <- AVL.delete k . AVL.unProof =<< AVL.prune set0 tree
            return (set0 == set1 && tree1 == tree2)
                :: StorageMonad Bool

        it' "Delete is idempotent" $ \(k, list) -> do
            tree  <- AVL.fromList list :: StorageMonad M
            tree1 <- AVL.deleteWithNoProof k tree
            tree2 <- AVL.deleteWithNoProof k tree1
            return (tree1 == tree2)
