
module Adapter (tests) where

import Control.Monad.Catch

import Common

import qualified Data.Tree.AVL.Adapter as AVL
import qualified Data.Tree.AVL         as Base

tests :: Spec
tests = describe "Adapter" do
    describe "transact" do
        it'' "rolls back" \(list, k, v) -> do
            Base.initialiseStorageIfNotAlready list

            _ <- AVL.proven () \() -> AVL.insert k v
            _ <- AVL.transact @SomeException do
                _ <- AVL.proven () \() -> do
                    AVL.insert k (v + 1)
                    throwM $ Base.NotFound ("b" :: StringName)
                return ()

            (res, _) <- AVL.proven () \() -> AVL.lookup k

            return $ res == Just v

        it'' "doesn't roll back when nothing is failed" \(list, k, v) -> do
            Base.initialiseStorageIfNotAlready list

            _ <- AVL.proven () \() -> AVL.insert k v
            _ <- AVL.transact @SomeException do
                _ <- AVL.proven () \() -> do
                    AVL.insert k (v + 1)
                return ()

            (res, _) <- AVL.proven () \() -> AVL.lookup k

            return $ res == Just (v + 1)

    describe "proven/prove" do
        it'' "is able to prove" \(list, k, v) -> do
            Base.initialiseStorageIfNotAlready list

            save     <- Base.currentRoot
            ((), tx) <- AVL.proven () \() -> AVL.insert k v

            Base.append save

            AVL.prove tx AVL.unpackServer \() -> AVL.insert k v

            return True

        it'' "isn't able to prove when incorrect" \(k, v, list) -> do
            Base.initialiseStorageIfNotAlready list

            save     <- Base.currentRoot
            ((), tx) <- AVL.proven () \() -> AVL.insert k v

            Base.append save

            ~(Left AVL.EndHashMismatch) <-
                AVL.transact @AVL.EndHashMismatch do
                    AVL.prove tx AVL.unpackClient \() -> do
                        AVL.insert k (v + 1)

            return True

    describe "rollback" do
        it'' "rolls transaction back" \(k, v, list) -> do
            Base.initialiseStorageIfNotAlready list

            old      <- Base.currentRoot
            ((), tx) <- AVL.proven () \() -> AVL.insert k v

            AVL.rollback tx AVL.unpackClient \() -> AVL.insert k v

            back <- Base.currentRoot

            return $ old == back

        it'' "keeps state if failed" \(k, v, list) -> do
            Base.initialiseStorageIfNotAlready list

            ((), tx)  <- AVL.proven () \() -> AVL.insert k v
            new       <- Base.currentRoot

            ~(Left _) <- AVL.transact @(Base.NotFound StringName) do
                AVL.rollback tx AVL.unpackClient \() -> do
                    AVL.insert k v
                    throwM $ Base.NotFound k

            back <- Base.currentRoot

            return $ new == back
