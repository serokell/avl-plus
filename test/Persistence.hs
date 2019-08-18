module Persistence (tests) where

import Data.List (sort)

import Common

import qualified Data.Tree.AVL as AVL

tests :: Spec
tests = describe "Persistence" $ do
    describe "Sanity check" $ do
        it'' "can rematerialise in Pure mutated storage after insert" $
            \(k, v, list) -> do
                AVL.genesis []
                tree       <- AVL.fromList         list
                ()         <- AVL.overwrite        tree
                (_, tree') <- AVL.insert       k v tree
                ()         <- AVL.overwrite        tree'
                full       <- AVL.currentRoot
                back       <- AVL.toList           full

                let uniq = uniqued (list ++ [(k, v)])

                return (back == uniq)

        it'' "can rematerialise in Pure mutated storage after delete" $
            \(k, list) -> do
                AVL.genesis []
                tree       <- AVL.fromList        list
                ()         <- AVL.overwrite       tree
                (_, tree') <- AVL.delete        k tree
                ()         <- AVL.overwrite       tree'
                full       <- AVL.currentRoot
                back       <- AVL.toList          full

                let uniq = uniqued (filter ((k /=) . fst) list)

                return (back == uniq)

        it'' "toList . initialiseStorageIfNotAlready ~ id" $
            \(kvs) -> do
                AVL.genesis []
                tree <- AVL.fromList    kvs
                ()   <- AVL.overwrite   tree
                root <- AVL.currentRoot
                lst  <- AVL.toList      root
                return (lst == sort (uniqued kvs))
