
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}

module Proof (tests) where

import Common

import qualified Debug.Trace   as Debug
import qualified Data.Tree.AVL as AVL

tests :: [Test]
tests =
    [ testGroup "Proofs"
        [ 
        testGroup "Insert"
            [ cachedProperty "Insert proof is verifiable" $ \(k, v, list) -> do
                tree            <- AVL.fromList list :: StorageMonad M
                (proof, tree1)  <- AVL.insert k v tree
                hash1           <- AVL.rootHash tree
                
                let AVL.Proof subtree = proof

                (proof1, _) <- AVL.withCacheLayer def $ do
                    AVL.insert k v subtree
                
                AVL.checkProof hash1 proof1

            , cachedProperty "Insert proof is replayable" $ \(k, v, list) -> do
                tree            <- AVL.fromList list :: StorageMonad M
                (proof, tree1)  <- AVL.insert k v tree
                hash1           <- AVL.rootHash tree1
                
                let AVL.Proof subtree = proof
                
                (proof1, tree2) <- AVL.withCacheLayer def $ do
                    AVL.insert k v subtree
                
                hash2 <- AVL.rootHash tree2

                lift $ when (hash1 /= hash2) $ do
                    print ("tree1", tree1)
                    print ("tree2", tree2)

                return (hash1 == hash2)
            ]

        , testGroup "Delete"
            [ cachedProperty "Delete proof is verifiable" $ \(k, v, list) -> do
                tree            <- AVL.fromList ((k, v) : list) :: StorageMonad M
                (proof, tree1)  <- AVL.delete k tree
                hash1           <- AVL.rootHash tree
                
                let AVL.Proof subtree = proof
                
                (proof1, tree2) <- AVL.withCacheLayer def $ do
                    AVL.delete k subtree

                hash2 <- AVL.rootHash tree2

                lift $ when (hash1 /= hash2) $ do
                    print ("hash1", hash1)
                    print ("hash2", hash2)

                AVL.checkProof hash1 proof1
            

        --    , cachedProperty "Delete proof is replayable" $ \(k, v, list) -> do
        --        tree            <- AVL.fromList ((k, v) : list) :: StorageMonad M
        --        (proof, tree1)  <- AVL.delete k tree
        --        hash1           <- AVL.rootHash tree1
                
        --        let AVL.Proof db root = proof
                
        --        (proof1, tree2) <- AVL.withCacheLayer db $ do
        --            AVL.delete k (AVL.ref root :: M)
                
        --        hash2 <- AVL.rootHash tree2

        --        lift $ when (hash1 /= hash2) $ do
        --            print ("tree1", tree1)
        --            print ("tree2", tree2)

        --        return (hash1 == hash2)

        --    , cachedProperty "Delete proof is verifiable (even if there's nothing to delete)" $ \list -> do
        --        case uniqued list of
        --          (k, v) : rest -> do
        --            tree            <- AVL.fromList rest :: StorageMonad M
        --            (proof, tree1)  <- AVL.delete k tree
        --            hash1           <- AVL.rootHash tree
                    
        --            let AVL.Proof db root = proof
                    
        --            (proof1, tree2) <- AVL.withCacheLayer db $ do
        --                AVL.delete k (AVL.ref root :: M)

        --            AVL.checkProof hash1 proof1
                  
        --          [] -> do
        --            return True
            ]
        ]
    ]
