
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}

module Algorithm (tests) where

import Common

import qualified Data.Tree.AVL as AVL

tests :: [Test]
tests =
    [ testGroup "Fold"
        [ cachedProperty "AVL.fold (x, f, end) (AVL.fromList list) ~ end (foldr f x (unique list))" $ \list -> do

            let start        = "tree:"
                f (k, v) acc = getStringName k ++ show v ++ ";" ++ acc
                end acc      = "[" ++ acc ++ "]"

            tree <- AVL.fromList list :: StorageMonad M
            res <- AVL.fold (start, f, end) tree

            let expected = end (foldr f start (reverse (uniqued list)))

            return (res == expected)
        ]
    ]
