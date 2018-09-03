module Unsafe (tests) where

import Data.List ((\\))

import Common

import qualified Data.Tree.AVL as AVL
import qualified Data.Tree.AVL.Store.Pure as Store
import qualified Data.Tree.AVL.Unsafe as Unsafe

tests :: Spec
tests = describe "Unsafe" $ do
    describe "Sanity check" $ do
        it'' "can rematerialise in Pure mutated storage after insert" $
            \(k :: StringName, v :: Int, list) -> do
                tree  <- AVL.fromList list :: Store.Store IntHash StringName Int StorageMonad M
                hash  <- AVL.save @IntHash tree
                ()    <- Unsafe.assignRoot @IntHash @StringName @Int hash
                _     <- Unsafe.mutateStorage $ AVL.insert @IntHash k v
                hash' <- Unsafe.getRoot    @IntHash @StringName @Int
                back  <- AVL.toList (AVL.ref @IntHash @StringName @Int hash')
                let uniq = uniqued ((k, v) : list)
                return (back == uniq)

        it'' "can rematerialise in Pure mutated storage after delete" $
            \(k :: StringName, v :: Int, list) -> do
                tree  <- AVL.fromList ((k, v) : list) :: Store.Store IntHash StringName Int StorageMonad M
                hash  <- AVL.save @IntHash tree
                ()    <- Unsafe.assignRoot @IntHash @StringName @Int hash
                _     <- Unsafe.mutateStorage $ AVL.delete @IntHash @_ @Int k
                hash' <- Unsafe.getRoot    @IntHash @StringName @Int
                back  <- AVL.toList (AVL.ref @IntHash @StringName @Int hash')
                let uniq = uniqued $ filter ((k /=) . fst) $ list
                unless (back == uniq) $ do
                    error $ show (back, uniq)
                return (back == uniq)
