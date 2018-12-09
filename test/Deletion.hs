module Deletion (tests) where

import Data.Foldable (for_)
import Data.Traversable (for)

import Data.List ((\\))

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

    it' "Deletion deletes" $ \list -> do
        if length list == 0
        then do
            return True

        else do
            let (k, _) : _  = list
            tree  <- AVL.fromList list :: StorageMonad M
            tree1 <- AVL.deleteWithNoProof k tree
            list' <- AVL.toList tree1
            let diff = uniqued list \\ list'
            return $ length diff == 1 && fst (head diff) == k

    describe "Proofs" $ do
        it' "Delete proof is verifiable" $ \(k, v, list) -> do

            tree        <- AVL.fromList ((k, v) : list) :: StorageMonad M
            (proof,  _) <- AVL.delete' k tree
            (proof1, _) <- AVL.delete' k (AVL.unProof proof)

            let Just hash1 = AVL.rootHash (AVL.fullRehash tree)

            return $ AVL.checkProof hash1 proof1

        it' "dpr Delete proof is replayable" $ \() -> do

            let (k, v, list) = (StringName "C", 0, [(StringName "R", 0)])

            tree        <- AVL.fromList ((k, v) : list) :: StorageMonad M
            (proof1, _) <- AVL.delete k tree
            (proof2, _) <- AVL.delete k . AVL.unProof =<< AVL.prune proof1 tree

            unless (proof1 == proof2) $ do
                put "=="
                put $ AVL.showMap tree
                put "--"
                put $ show proof1
                put "--"
                put $ show proof2

            return (proof1 == proof2)
                :: StorageMonad Bool

        it' "Delete proof is verifiable (even if there's nothing to delete)" $ \list -> do
            case uniqued list of
              (k, _) : rest -> do
                tree        <- AVL.fromList rest :: StorageMonad M
                (proof,  _) <- AVL.delete' k tree
                (proof1, _) <- AVL.delete' k (AVL.unProof proof)

                let Just hash1 = AVL.rootHash (AVL.fullRehash tree)

                return $ AVL.checkProof hash1 proof1

              [] -> do
                return True

        it' "Delete is idempotent" $ \(k, list) -> do
            tree <- AVL.fromList list :: StorageMonad M
            (_, tree1) <- AVL.delete k tree
            (_, tree2) <- AVL.delete k tree1
            return (tree1 == tree2)
