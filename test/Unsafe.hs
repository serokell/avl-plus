module Unsafe (tests) where

import Data.List (sort)

import Common

import qualified Data.Tree.AVL as AVL
import qualified Data.Tree.AVL.Store.Pure as Pure

tests :: Spec
tests = describe "Unsafe" $ do
    describe "Sanity check" $ do
        it'' "can rematerialise in Pure mutated storage after insert" $
            \(k :: StringName, v :: Int, list) -> do
                AVL.initialiseStorageIfNotAlready @IntHash @StringName @Int []
                tree       <- AVL.fromList list :: Pure.StoreT IntHash StringName Int StorageMonad M
                ()         <- AVL.overwrite        tree
                (_, tree') <- AVL.insert       k v tree
                ()         <- AVL.overwrite        tree'
                full       <- AVL.currentRoot
                back       <- AVL.toList @IntHash  full

                let uniq = uniqued (list ++ [(k, v)])

                return (back == uniq)

        it'' "can rematerialise in Pure mutated storage after delete" $
            \(k :: StringName, list) -> do
                AVL.initialiseStorageIfNotAlready @IntHash @StringName @Int []
                tree       <- AVL.fromList list :: Pure.StoreT IntHash StringName Int StorageMonad M
                ()         <- AVL.overwrite       tree
                (_, tree') <- AVL.delete        k tree
                ()         <- AVL.overwrite       tree'
                full       <- AVL.currentRoot
                back       <- AVL.toList @IntHash full

                let uniq = uniqued (filter ((k /=) . fst) list)

                return (back == uniq)

        it'' "toList . initialiseStorageIfNotAlready ~ id" $
            \(kvs) -> do
                AVL.initialiseStorageIfNotAlready @IntHash @StringName @Int []
                tree <- AVL.fromList kvs :: Pure.StoreT IntHash StringName Int StorageMonad M
                ()   <- AVL.overwrite tree
                root <- AVL.currentRoot :: Pure.StoreT IntHash StringName Int StorageMonad M
                lst  <- AVL.toList root
                return (lst == sort (uniqued kvs))
