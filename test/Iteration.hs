
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE OverloadedStrings     #-}

module Iteration (tests) where

import Common

import qualified Data.Tree.AVL as AVL

checkList :: [(StringName, Int)] -> M -> AVL.IteratedT Int StorageMonad ()
checkList list tree = do
    AVL.startIteration tree
    go list
  where
    go (pair : others) = do
        AVL.continueIteration @Int >>= \case
            Just pair' -> do
                -- liftIO (print (pair, pair'))
                when (pair /= pair') $ do
                    error "Eh"
                go others

            Nothing -> do
                error "oh"

    go [] = return ()

tests :: Spec
tests = describe "Iteration" $ do
    describe "Full resumable iteration" $ do
        it' "collectAll ~ toList" $ \list -> do
            let
              -- list =
              --   [("B",-13),("I",1),("G",-12)
              --   ,("D",-16)
              --   ,("C",13)]

              -- list =
              --   [("B",-13),("C",1),("D",3),("G",22)]

            tree  <- AVL.fromList list :: StorageMonad M
            list' <- AVL.toList tree

            putStrLn "Iteration..."
            putStrLn $ AVL.showMap tree
            AVL.runIteratedT (checkList list' tree)

            return True
