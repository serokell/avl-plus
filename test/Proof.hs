
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}

module Proof (tests) where

import Common

import qualified Debug.Trace as Debug

import qualified Data.Tree.AVL            as AVL

--
import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck                      ( Arbitrary (..)
                                                      , Gen
                                                      , Property
                                                      , (===)
                                                      , (==>) )
import           Test.QuickCheck.Instances  ()

tests :: [Test]
tests =
    [ testGroup "Proofs"
        [ testProperty "Generated proofs are verified" $
          \k v list ->
            let
                tree           = AVL.fromList list :: M
                hash0          = tree^.AVL.rootHash
                (proof, tree1) = AVL.insert k v tree
            in
                AVL.checkProof hash0 proof

        , testProperty "Insert proof is verifiable" $
          \k v list ->
            let
                tree            = AVL.fromList list :: M
                (proof, tree1)  = AVL.insert k v tree
                hash1           = tree1^.AVL.rootHash
                AVL.Proof spine = proof
                (_, inserted)   = AVL.insert k v spine
            in
                AVL.checkProof hash1 (AVL.Proof inserted)

        , testProperty "Delete proof is verifiable" $
          \k v list ->
            let
                tree            = AVL.fromList ((k, v) : list) :: M
                (proof, tree1)  = AVL.delete k tree
                hash1           = tree1^.AVL.rootHash
                AVL.Proof spine = proof
                (_, deleted)    = AVL.delete k spine

                check = AVL.checkProof hash1 (AVL.Proof deleted)
            in
                check
        ]
    ]
