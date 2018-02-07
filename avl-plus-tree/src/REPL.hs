
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module REPL (module M) where

import Control.Arrow (first)
import Control.Lens  ((^.))
import Control.Monad (foldM, unless)

import Data.Bits (xor)
import Data.List (sort)
import Data.Monoid

import Data.Tree.AVL as M

data InitialHash
    = HashOf (StringName, Int, StringName, StringName)
    | Combine (InitialHash, Side, Tilt, InitialHash)
    | EmptyOne

instance Eq InitialHash where
    x == y = show x == show y

instance Show InitialHash where
    show = \case
      HashOf  (k, _, p, n) -> show (p, k, n)
      Combine (l, L, t, r) -> "(" <> show l <> " # " <> show t <> " # " <> show r <> ")"
      Combine (l, R, t, r) -> "(" <> show r <> " # " <> show t <> " # " <> show l <> ")"
      EmptyOne             -> "0"

instance Combined InitialHash where
    emptyOne = EmptyOne
    combine  = Combine

instance Hash InitialHash StringName Int where
    hashOf = HashOf

newtype StringName = StringName { getStringName :: String }
    deriving (Eq, Ord)

instance Show StringName where
    show = getStringName

instance Bounded StringName where
    minBound = StringName "A"
    maxBound = StringName "Z"


type M = M.Map InitialHash StringName Int

l = map (first StringName) [("I",0),("L",0), ("Q", 0)]

x = M.fromList l :: M

main () = do
    foldM push (M.empty :: M) l
    return ()
  where
    push (tree) (k, v) = do
        putStrLn ("INSERING " <> show k)
        let (proof, tree1) = M.insert k v tree
        -- print tree1
        -- print (tree1^.rootHash)
        unless (checkProof proof (tree^.rootHash)) $ do
            error "foo"
        return tree1
