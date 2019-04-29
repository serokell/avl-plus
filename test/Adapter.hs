
module Adapter (tests) where

import Control.Monad.Catch
import Control.Monad.Reader

import Common

import qualified Data.Tree.AVL.Adapter as AVL
import qualified Data.Tree.AVL         as Base

tests :: Spec
tests = describe "Adapter" do
    describe "transact" do
        it'' "rolls back" \() -> do
            Base.initialiseStorageIfNotAlready
                [ ("a", 5 :: Int)
                ]

            _ <- AVL.transact @SomeException do
                AVL.proven () \() -> do
                    AVL.insert "a" 6
                    throwM $ Base.NotFound ("b" :: StringName)

            (res, _) <- AVL.proven () \() -> do
                AVL.lookup "a"

            return $ res == Just 5

        it'' "doesn't roll back when nothing is failed" \() -> do
            Base.initialiseStorageIfNotAlready
                [ ("a", 5 :: Int)
                ]

            _ <- AVL.transact @SomeException do
                AVL.proven () \() -> do
                    AVL.insert "a" 6

            (res, _) <- AVL.proven () \() -> do
                AVL.lookup "a"

            return $ res == Just 6

    describe "proven/prove" do
        it'' "is able to prove" \() -> do
            Base.initialiseStorageIfNotAlready []

            save <- Base.currentRoot

            ((), tx) <- AVL.proven () \() -> do
                AVL.insert "x" 42

            Base.append save

            AVL.prove tx AVL.unpackServer \() -> do
                AVL.insert "x" 42

            return True

        it'' "isn't able to prove when incorrect" \() -> do
            Base.initialiseStorageIfNotAlready []

            save <- Base.currentRoot

            ((), tx) <- AVL.proven () \() -> do
                AVL.insert "x" 42

            Base.append save

            ~(Left AVL.EndHashMismatch) <-
                AVL.transact @AVL.EndHashMismatch do
                    AVL.prove tx AVL.unpackServer \() -> do
                        AVL.insert "x" 43

            return True

    describe "rollback" do
        it'' "rolls transaction back" \() -> do
            Base.initialiseStorageIfNotAlready []

            old <- Base.currentRoot

            ((), (a, b, c)) <- AVL.proven () \() -> do
                AVL.insert "x" 42

            new <- Base.currentRoot

            AVL.rollback (a, b, c) AVL.unpackServer \() -> do
                AVL.insert "x" 42

            back <- Base.currentRoot

            liftIO $ print (old, b)

            return $ old == back
