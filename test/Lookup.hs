
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}

module Lookup (tests) where

import Common

import qualified Data.Tree.AVL as AVL

tests :: [Test]
tests =
    [ testGroup "Lookup"
        [ cachedProperty "Generated proofs are verified" $ \(k, list) -> do
            tree            <- AVL.fromList list :: StorageMonad M
            ((_, proof), _) <- AVL.lookup k tree

            return $ AVL.checkProof (AVL.rootHash tree) proof

        , cachedProperty "Generated proofs are replayable" $ \(k, list) -> do
            tree            <- AVL.fromList list :: StorageMonad M
            ((_, proof), _) <- AVL.lookup k tree

            let AVL.Proof subtree = proof

            _ <- AVL.sandboxed $ do
                AVL.lookup k subtree

            return $ AVL.checkProof (AVL.rootHash tree) proof

        , cachedProperty "Lookup actually works" $ \(list) -> do
            case uniqued list of
              [] -> do
                return True

              (k, v) : rest -> do
                tree              <- AVL.fromList ((k, v) : rest) :: StorageMonad M
                ((Just v1, _), _) <- AVL.lookup k tree

                return (v == v1)
        ]
    ]
