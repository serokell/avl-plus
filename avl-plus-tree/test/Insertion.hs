
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}

module Insertion (tests) where

import           Data.Ord (comparing)
import           Data.Function (on)
import           Data.List (sortBy, nubBy, (\\))

import qualified Debug.Trace as Debug

import qualified Data.Tree.AVL            as AVL

import           Test.Framework             (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck            (Arbitrary (..), Gen, Property,
                                             (===), (==>))
import           Test.QuickCheck.Instances  ()

import           Common

unique = nubBy ((==) `on` fst)
uniqued = sortBy (comparing fst) . unique . reverse

prettyMuchBalanced :: Float -> M -> Bool
prettyMuchBalanced delta tree =
    AVL.size tree == 0 ||
    let
        lengths =
            AVL.pathLengths tree

        averagePath =
            fromIntegral (sum lengths) / fromIntegral (length lengths) :: Float

        sizeLog2plus =
            log (fromIntegral (AVL.size tree + 1)) / log 2 * (1 + delta)
    in
        averagePath <= sizeLog2plus

tests :: [Test]
tests =
    [ testGroup "Order check"
        [ testProperty "toList . fromList == sort . unique" $ \list ->
            let
                tree = AVL.fromList list :: M
                back = AVL.toList tree
                uniq = uniqued list
            in
                back == uniq
        ]
    , testGroup "Rebalance quality"
        [ testProperty
            "forall tree, avg. height tree <= log2 (size tree) * 1.05" $
            prettyMuchBalanced 0.05 . AVL.fromList
        ]
    , testGroup "Deletion"
        [ testProperty "deletion keeps balance" $
          \list ->
            let
                tree = AVL.fromList list
                trees = scanl (flip (AVL.deleteWithNoProof . fst)) tree list
            in if
                all (prettyMuchBalanced 0.06) trees
            then True else
              Debug.traceShow list False
        , testProperty "deletion is sane" $
          \list -> length list == 0 ||
            let
                (k, _) : _  = list
                tree   = AVL.fromList list :: M
                tree1  = AVL.deleteWithNoProof k tree
                list'  = AVL.toList tree1
                diff   = uniqued list \\ list'
            in
                length diff == 1 && fst (head diff) == k
        ]
    ]
