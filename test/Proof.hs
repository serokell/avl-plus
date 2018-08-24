
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}

module Proof (tests) where

import Common

import qualified Data.Tree.AVL as AVL

tests :: Spec
tests = describe "Proofs" $ do
    describe "Insert" $ do
        it' "Insert proof is verifiable" $ \(k, v, list) -> do
            tree        <- AVL.fromList list :: StorageMonad M
            (proof, _)  <- AVL.insert k v tree
            let hash1    = AVL.rootHash tree

            let AVL.Proof subtree = proof

            (proof1, _) <- AVL.insert k v subtree

            return $ AVL.checkProof hash1 proof1

        it' "Insert proof is replayable" $ \(k, v, list) -> do
            tree            <- AVL.fromList list :: StorageMonad M
            (proof, tree1)  <- AVL.insert k v tree
            let hash1        = AVL.rootHash tree1

            let AVL.Proof subtree = proof

            (_, tree2) <- AVL.insert k v subtree

            let hash2        = AVL.rootHash tree1

            when (hash1 /= hash2) $ do
                print ("tree1", tree1)
                print ("tree2", tree2)

            return (hash1 == hash2 && tree1 == tree2)

    describe "Delete" $ do
        it' "Delete proof is verifiable" $ \(k, v, list) -> do
            tree        <- AVL.fromList ((k, v) : list) :: StorageMonad M
            let hash1    = AVL.rootHash tree
            (proof, _)  <- AVL.delete k tree

            let AVL.Proof subtree = proof

            (proof1, _) <- AVL.delete k subtree

            return $ AVL.checkProof hash1 proof1

        it' "Delete proof is replayable" $ \(k, v, list) -> do
            tree            <- AVL.fromList ((k, v) : list) :: StorageMonad M
            (proof, tree1)  <- AVL.delete k tree
            let hash1        = AVL.rootHash tree1

            let AVL.Proof subtree = proof

            (_, tree2) <- AVL.delete k subtree

            let hash2 = AVL.rootHash tree2

            when (hash1 /= hash2) $ do
                print ("tree1", tree1)
                print ("tree2", tree2)

            return (hash1 == hash2 && tree1 == tree2)

        it' "Delete proof is verifiable (even if there's nothing to delete)" $ \list -> do
            case uniqued list of
              (k, _) : rest -> do
                tree        <- AVL.fromList rest :: StorageMonad M
                (proof, _)  <- AVL.delete k tree
                let hash1    = AVL.rootHash tree

                let AVL.Proof subtree = proof

                (proof1, _) <- AVL.delete k subtree

                return $ AVL.checkProof hash1 proof1

              [] -> do
                return True
