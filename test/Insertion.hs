
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}

module Insertion (tests) where

import Universum (allM, for_)

import Data.List ((\\))

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

    describe "Deletion" $ do
        it' "Tree is still balanced after delete" $ \list -> do
            tree  <- AVL.fromList list :: StorageMonad M
            trees <- scanM (AVL.deleteWithNoProof . fst) tree list
            yes   <- allM  (AVL.isBalancedToTheLeaves)   trees

            for_ trees $ AVL.isBalancedToTheLeaves

            return yes

        it' "Deletion deletes" $ \list -> do
            if length list == 0
            then do
                return True

            else do
                let (k, _) : _  = list
                tree  <- AVL.fromList list :: StorageMonad M
                tree1 <- AVL.deleteWithNoProof k tree
                list' <- AVL.toList tree1
                let diff = uniqued list \\ list'
                return $ length diff == 1 && fst (head diff) == k
