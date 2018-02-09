
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
        [ testProperty "Generated proofs are verified." $
          \k v list ->
            let
                tree           = AVL.fromList list :: M
                hash0          = tree^.AVL.rootHash
                (proof, tree1) = AVL.insert k v tree
            in
              Debug.traceShow ("proof", proof) $
              Debug.traceShow ("hash0", hash0) $
                AVL.checkProof hash0 proof
        ]
    ]
