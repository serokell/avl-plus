module Unsafe (tests) where

import Data.List ((\\))

import Common

import qualified Data.Tree.AVL as AVL
import qualified Data.Tree.AVL.Store.Pure as Store
import qualified Data.Tree.AVL.Unsafe as AVL

tests :: Spec
tests = describe "Unsafe" $ do
    describe "Sanity check" $ do
        it'' "can rematerialise in Pure mutated storage after insert" $
            \(k :: StringName, v :: Int, list) -> do
                tree           <- AVL.fromList list :: Store.Store IntHash StringName Int StorageMonad M
                ()             <- AVL.overwrite        tree
                (proof, tree') <- AVL.insert       k v tree
                ()             <- AVL.overwrite        tree'
                full           <- AVL.currentRoot
                back           <- AVL.toList @IntHash  full

                let uniq = uniqued (list ++ [(k, v)])

                return (back == uniq)

        it'' "can rematerialise in Pure mutated storage after delete" $
            \(k :: StringName, list) -> do
                tree           <- AVL.fromList list :: Store.Store IntHash StringName Int StorageMonad M
                ()             <- AVL.overwrite       tree
                (proof, tree') <- AVL.delete        k tree
                ()             <- AVL.overwrite       tree'
                full           <- AVL.currentRoot
                back           <- AVL.toList @IntHash full

                let uniq = uniqued (filter ((k /=) . fst) list)

                return (back == uniq)
