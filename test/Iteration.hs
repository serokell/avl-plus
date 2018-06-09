
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}

module Iteration (tests) where

import Common

import           Data.Proxy
import qualified Data.Tree.AVL as AVL

collectAll :: M -> AVL.IteratedT Int StorageMonad [(StringName, Int)]
collectAll tree = do
    AVL.startIteration tree
    go
  where
    go = do
        AVL.continueIteration (Proxy :: Proxy Int) >>= \case
            Just (k, v) -> do
                rest <- go
                return $ (k, v) : rest

            Nothing -> do
                return []

tests :: [Test]
tests =
    [ testGroup "Iteration"
        [ testGroup "Full resumable iteration"
            [ cachedProperty "collectAll ~ toList" $ \list -> do

                tree   <- AVL.fromList list :: StorageMonad M
                list'  <- AVL.runIteratedT (collectAll tree)
                list'' <- AVL.toList tree

                return (list' == list'')
            ]
        ]
    ]
