
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}

module Lookup (tests) where

import Common

import qualified Data.Tree.AVL as AVL
import qualified Debug.Trace   as Debug

tests :: [Test]
tests =
    [ testGroup "Lookup"
        [ cachedProperty "Generated proofs are verified" $ \(k, list) -> do
            tree                 <- AVL.fromList list :: StorageMonad M
            ((search, proof), _) <- AVL.lookup k tree
            hash1                <- AVL.rootHash tree
            AVL.checkProof hash1 proof

        , cachedProperty "Generated proofs are replayable" $ \(k, list) -> do
            tree                 <- AVL.fromList list :: StorageMonad M
            ((search, proof), _) <- AVL.lookup k tree
            hash1                <- AVL.rootHash tree

            let AVL.Proof subtree = proof

            search1 <- AVL.sandboxed $ do
                AVL.lookup k subtree

            AVL.checkProof hash1 proof
        ]
    ]
