
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}

module Proof (tests) where

import Common

import qualified Data.Tree.AVL as AVL

tests :: [Test]
tests =
    [ testGroup "Proofs"
        [
        testGroup "Insert"
            [ cachedProperty "Insert proof is verifiable" $ \(k, v, list) -> do
                tree        <- AVL.fromList list :: StorageMonad M
                (proof, _)  <- AVL.insert k v tree
                let hash1    = AVL.rootHash tree

                let AVL.Proof subtree = proof

                (proof1, _) <- AVL.sandboxed $ do
                    AVL.insert k v subtree

                AVL.checkProof hash1 proof1

            , cachedProperty "Insert proof is replayable" $ \(k, v, list) -> do
                tree            <- AVL.fromList list :: StorageMonad M
                (proof, tree1)  <- AVL.insert k v tree
                let hash1        = AVL.rootHash tree1

                let AVL.Proof subtree = proof

                (_, tree2) <- AVL.sandboxed $ do
                    AVL.insert k v subtree

                let hash2        = AVL.rootHash tree1

                lift $ when (hash1 /= hash2) $ do
                    print ("tree1", tree1)
                    print ("tree2", tree2)

                return (hash1 == hash2)
            ]

        ,
        testGroup "Delete"
            [ cachedProperty "Delete proof is verifiable" $ \(k, v, list) -> do
                tree        <- AVL.fromList ((k, v) : list) :: StorageMonad M
                let hash1    = AVL.rootHash tree
                (proof, _)  <- AVL.delete k tree

                let AVL.Proof subtree = proof

                (proof1, _) <- AVL.sandboxed $ do
                    AVL.delete k subtree

                yes <- AVL.checkProof hash1 proof1
                return yes

            , cachedProperty "Delete proof is replayable" $ \(k, v, list) -> do
                tree            <- AVL.fromList ((k, v) : list) :: StorageMonad M
                (proof, tree1)  <- AVL.delete k tree
                let hash1        = AVL.rootHash tree1

                let AVL.Proof subtree = proof

                (_, tree2) <- AVL.sandboxed $ do
                    AVL.delete k subtree

                let hash2        = AVL.rootHash tree2

                lift $ when (hash1 /= hash2) $ do
                    print ("tree1", tree1)
                    print ("tree2", tree2)

                return (hash1 == hash2)

            , cachedProperty "Delete proof is verifiable (even if there's nothing to delete)" $ \list -> do
                case uniqued list of
                  (k, _) : rest -> do
                    tree        <- AVL.fromList rest :: StorageMonad M
                    (proof, _)  <- AVL.delete k tree
                    let hash1    = AVL.rootHash tree

                    let AVL.Proof subtree = proof

                    (proof1, _) <- AVL.sandboxed $ do
                        AVL.delete k subtree

                    AVL.checkProof hash1 proof1

                  [] -> do
                    return True
            ]
        ]
    ]
