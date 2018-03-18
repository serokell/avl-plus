
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}

module Insertion (tests) where

import Universum (allM, when, lift)

import Data.Ord                             (comparing)
import Data.Function                        (on)
import Data.List                            (sortBy, nubBy, (\\))

import Common

import qualified Data.Tree.AVL as AVL
import qualified Debug.Trace   as Debug

unique  = nubBy  ((==) `on` fst)
uniqued = sortBy (comparing fst) . unique . reverse

prettyMuchBalanced :: Float -> M -> StorageMonad Bool
prettyMuchBalanced delta tree = do
    size <- AVL.size tree
    if size == 0
    then do
        return True

    else do
        lengths <- AVL.pathLengths tree
        size    <- AVL.size tree
        let
          cast :: (Integral a, Num b) => a -> b
          cast = fromIntegral
          
          averagePath  = cast (sum lengths) / cast (length lengths) :: Float
          sizeLog2plus = log (cast (size + 1)) / log 2 * (1 + delta)

        return $ averagePath <= sizeLog2plus

tests :: [Test]
tests =
    [ testGroup "Order check"
        [ cachedProperty "toList . fromList == sort . unique" $ \list -> do
            tree <- AVL.fromList list :: StorageMonad M
            back <- AVL.toList tree
            let uniq = uniqued list
            return (back == uniq)
        ]
    , testGroup "Rebalance quality"
        [ cachedProperty
            "forall tree, avg. height tree <= log2 (size tree) * 1.05" $ \list -> do
                tree <- AVL.fromList list :: StorageMonad M
                prettyMuchBalanced 0.05 tree
        ]

    , testGroup "Pruning preserves hash"
        [ cachedProperty "It does" $ \list -> do
            tree  <- AVL.fromList list :: StorageMonad M
            hash1 <- AVL.rootHash tree
            hash2 <- AVL.rootHash (AVL.pruned tree)
            return $ hash1 == hash2
        ]

    , testGroup "Deletion"
        [ cachedProperty "deletion keeps balance" $ \list -> do
            tree  <- AVL.fromList list
            trees <- scanM (AVL.deleteWithNoProof . fst) tree list
            yes   <- allM (prettyMuchBalanced 0.06) trees
            
            when (not yes) $ do
                lift $ print list
            
            return yes
        
        , cachedProperty "deletion is sane" $ \list -> do
            if length list == 0
            then do
                return True
            
            else do
                let (k, _) : _  = list
                tree   <- AVL.fromList list :: StorageMonad M
                tree1  <- AVL.deleteWithNoProof k tree
                list'  <- AVL.toList tree1
                let diff   = uniqued list \\ list'
                return $ length diff == 1 && fst (head diff) == k
        ]
    ]
