
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}

module Proof (tests) where

import Common

import qualified Debug.Trace as Debug

import qualified Data.Tree.AVL            as AVLPlus

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
                tree           = AVLPlus.fromList list :: M
                hash0          = tree^.AVLPlus.rootHash
                (proof, tree1) = AVLPlus.insert k v tree
            in
                AVLPlus.checkProof proof hash0
        ]
    ]
