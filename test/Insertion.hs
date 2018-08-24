
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}

module Insertion (tests) where

import Common

import qualified Data.Tree.AVL as AVL

tests :: Spec
tests = describe "Insert" $ do
    it' "toList . fromList == sort . unique" $ \list -> do
        tree <- AVL.fromList list :: StorageMonad M
        back <- AVL.toList tree
        let uniq = uniqued list
        return (back == uniq)

    it' "Tree is as balanced as possible" $ \list -> do
        tree <- AVL.fromList list :: StorageMonad M
        AVL.isBalancedToTheLeaves tree

    describe "Proofs" $ do
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

