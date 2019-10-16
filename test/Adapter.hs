
module Adapter (tests) where

import Common

import qualified Data.Blockchain.Storage.AVL as AVL
import qualified Data.Tree.AVL               as Base

tests :: Spec
tests = describe "Adapter" do
    describe "transact" do
        uit'' "rolls back" \(list, k :: StringName, v) -> do
            AVL.genesis list

            _ <- AVL.autoCommit do
                AVL.recordProof () \() -> do
                    AVL.insert k v

            _ <- AVL.try $ AVL.autoCommit do
                _ <- AVL.recordProof () \() -> do
                    AVL.insert k (v + 1)
                    Base.notFound k
                return ()

            (res, _) <- AVL.autoCommit $ do
                AVL.recordProof () \() -> do
                    AVL.lookup k

            return $ res == Just v

        uit'' "doesn't roll back when nothing is failed"
            \(list, k :: StringName, v) -> do
                AVL.genesis list

                _ <- AVL.autoCommit do
                    AVL.recordProof () \() -> do
                        AVL.insert k v

                _ <- AVL.autoCommit do
                    _ <- AVL.recordProof () \() -> do
                        AVL.insert k (v + 1)
                    return ()

                (res, _) <- AVL.autoCommit do
                    AVL.recordProof () \() -> do
                        AVL.lookup k

                return $ res == Just (v + 1)

    describe "record/apply" do
        uit'' "is able to apply" \(list, k :: Bool, v) -> do
            AVL.genesis list

            save     <- Base.currentRoot
            ((), tx) <- AVL.autoCommit do
                AVL.recordProof () \() -> do
                    AVL.insert k v

            Base.append save

            AVL.autoCommit do
                AVL.apply tx \() -> do
                    AVL.insert k v

            return True

        uit'' "isn't able to apply when incorrect"
            \(k :: StringName, v, list) -> do
                AVL.genesis list

                save     <- Base.currentRoot
                ((), tx) <- AVL.autoCommit do
                    AVL.recordProof () \() -> do
                        AVL.insert k v

                Base.append save

                ~(Left _) <-
                    AVL.try $ AVL.autoCommit do
                        AVL.apply tx \() -> do
                            AVL.insert k (v + 1)

                return True

    describe "rollback" do
        uit'' "rolls transaction back" \(k :: Bool, v, list) -> do
            AVL.genesis list

            old      <- Base.currentRoot
            ((), tx) <- AVL.autoCommit do
                AVL.recordProof () \() -> do
                    AVL.insert k v

            AVL.autoCommit do
                AVL.rollback tx \() -> do
                    AVL.insert k v

            back <- Base.currentRoot

            return $ old == back

        uit'' "keeps state if failed" \(k :: StringName, v, list) -> do
            AVL.genesis list

            ((), tx)  <- AVL.autoCommit do
                AVL.recordProof () \() -> do
                    AVL.insert k v

            new <- Base.currentRoot

            ~(Left _) <- AVL.try $ AVL.autoCommit do
                AVL.rollback tx \() -> do
                    AVL.insert k v
                    Base.notFound k

            back <- Base.currentRoot

            return $ new == back
