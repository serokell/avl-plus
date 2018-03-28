
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}

module Cache (tests) where

import Common

import qualified Debug.Trace   as Debug
import qualified Data.Tree.AVL as AVL

tests :: [Test]
tests =
    [ testGroup "Cache"
        [ cachedProperty "The `sandboxed` sandboxes" $ \(list) -> do
            tree <- AVL.fromList list :: StorageMonad M
            AVL.save tree

            AVL.sandboxed $ do
                _ <- AVL.retrieve (AVL.rootHash tree) :: StorageMonad Layer
                return False
              `catch` \(AVL.NotFound (_ :: Int)) ->
                return True

        , cachedProperty "The `inCacheLayer` allows to look through it" $ \(list) -> do
            tree <- AVL.fromList list :: StorageMonad M
            AVL.save tree

            (yes, _) <- AVL.runOnEmptyCache $ do
                _ <- AVL.retrieve (AVL.rootHash tree) :: CachedStorageMonad Layer
                return True
              `catch` \(AVL.NotFound (_ :: Int)) ->
                return False

            return yes
        ]
    ]
