
{-# language NamedFieldPuns #-}
{-# language TemplateHaskell #-}

module Data.Tree.AVL.Proof where

import Control.Applicative
import Control.Lens hiding (locus, Empty)
import Control.Monad.State.Strict

import Debug.Trace as Debug

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Zipper

data Proof h k v
    = Proof { getProof :: [(Tilt, Side, h)], end :: (k, v, k, k) }
    | TreeWasEmpty
    | TrustMe
    deriving Show

checkProof :: Hash h k v => Proof h k v -> h -> Bool
checkProof proof ideal = case proof of
  Proof path end ->
    let weHave = foldr joinHashes (hashOf end) path
    in
        ideal == weHave
  TreeWasEmpty   -> ideal == emptyOne
  TrustMe        -> True
  where
    joinHashes (t, L, right) left = combine (left, L, t, right)
    joinHashes (t, R, left) right = combine (left, L, t, right)

trackProof :: Hash h k v => Zipped h k v (Proof h k v)
trackProof = do
    tree <- use locus
    case tree of
      Empty {} ->
        return TreeWasEmpty

      _ -> do
        datum <- leafDataForProof
        path  <- collectProof
        return (Proof (drop 1 (reverse path)) datum)

  where
    collectProof = do
        side <- up
        tree <- use locus
        case tree of
          Branch {} -> do
            let
              child = if other side == L then aptLeft else aptRight
              hash = tree^?!child.rootHash
            tilt <- use (locus.tilt)
            rest <- collectProof
            return $ (tilt, side, hash) : rest
          _ -> do
            return []
      <|> do
        return []

leafDataForProof :: Hash h k v => Zipped h k v (k, v, k, k)
leafDataForProof = do
    tree <- use locus
    case tree of
      Leaf {} -> do
        return
            ( tree^?!aptKey
            , tree^?!aptValue
            , tree^?!aptPrevKey
            , tree^?!aptNextKey
            )
      _ ->
        fail "We're not in leaf"
