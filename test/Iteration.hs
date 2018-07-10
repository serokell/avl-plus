
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE OverloadedStrings     #-}

module Iteration (tests) where

import Common

import           Data.Proxy
import qualified Data.Tree.AVL as AVL

checkList :: [(StringName, Int)] -> M -> AVL.IteratedT Int StorageMonad ()
checkList list tree = do
    AVL.startIteration tree
    go list
  where
    go (pair : others) = do
        AVL.continueIteration (Proxy :: Proxy Int) >>= \case
            Just pair' -> do
                -- liftIO (print (pair, pair'))
                when (pair /= pair') $ do
                    error "Eh"
                go others

            Nothing -> do
                error "oh"

    go [] = return ()

tests :: [Test]
tests =
    [ testGroup "Iteration"
        [ testGroup "Full resumable iteration"
            [ cachedProperty "collectAll ~ toList" $ \list -> do

                let
                  -- list =
                  --   [("B",-13),("I",1),("G",-12)
                  --   ,("D",-16)
                  --   ,("C",13)]

                  -- list =
                  --   [("B",-13),("C",1),("D",3),("G",22)]

                tree  <- AVL.fromList list :: StorageMonad M
                list' <- AVL.toList tree

                -- liftIO $ putStrLn $ AVL.showMap tree
                AVL.runIteratedT (checkList list' tree)

                return True
            ]
        ]
    ]
