
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE DeriveGeneric  #-}

--module REPL where

import Control.Lens hiding (locus, elements, Empty)

import           Data.Bits (xor)
import           Data.Default
import           Data.Hashable
import           Data.Foldable
import           Data.Function (on)
import           Data.List (sortBy, nubBy)
import           Data.Monoid
import           Data.Ord (comparing)
import           Data.String (IsString(..))

import           GHC.Generics (Generic)

import qualified Data.Tree.AVL            as AVL

data InitialHash
    = InitialHash { getInitialHash :: Layer }
    | Default
    deriving (Ord, Generic)

type Layer = AVL.MapLayer InitialHash StringName Int InitialHash

instance Hashable InitialHash

deriving instance Ord Layer
instance Hashable Layer

deriving instance Generic  StringName
instance Hashable StringName

instance Hashable AVL.Tilt

instance Show InitialHash where
    show = \case
        InitialHash m -> "#(" ++ show (m & AVL.mlHash .~ Default) ++ ")"
        Default       -> "#DEFAULT"

instance Default InitialHash where
  def = Default

instance Eq InitialHash where
    x == y = show x == show y

instance AVL.Hash InitialHash StringName Int where
    hashOf tree = case tree of
        other -> InitialHash tree

-- newtype IntHash = IntHash { getIntHash :: Int }
--     deriving (Show, Eq, Arbitrary)
--
newtype StringName = StringName { getStringName :: String }
    deriving (Eq, Ord)

instance Show StringName where
    show = getStringName

instance Bounded StringName where
    minBound = StringName "A"
    maxBound = StringName "Z"

instance IsString StringName where
    fromString = StringName

-- instance AVLPlus.Combined IntHash where
--     emptyOne = IntHash 0
--     combine (IntHash x, t, IntHash y) =
--         IntHash (x * 67 + fromEnum t * 79 + y * 121)
--
-- instance AVLPlus.Hash IntHash StringName Int where
--     hashOf
--         ( StringName k
--         , v
--         , StringName p
--         , StringName n )
--       =
--         IntHash $ 37 * length k + 53 * v + 67 * length p + 91 * length n

--instance
--    ( AVL.Hash h k v
--    , Arbitrary k
--    , Arbitrary v
--    , Show h
--    )
--      =>
--    Arbitrary (AVL.Map h k v)
--  where
--    arbitrary = AVL.fromList <$> arbitrary

-- Requirement of QuickCheck
instance Show (a -> b) where
    show _ = "<function>"

type StorageMonad = AVL.HashMapStore InitialHash StringName Int AVL.NullStore

type M = AVL.Map InitialHash StringName Int

-- Replayability of insert proof fails on:
-- (O,-12,[(O,21),(S,-63),(U,42)])
-- (J,-68,[(E,44),(T,-30),(V,-86),(O,-38),(Q,-84),(T,67),(O,32),(Y,26),(B,58),(Q,-71),(G,-63),(D,23),(W,83),(L,-51),(P,43),(E,31),(J,63),(P,69),(V,79),(B,8),(N,28),(T,1),(Q,-44),(T,-91),(L,61),(X,-10),(Q,87),(T,64),(V,61),(O,89),(G,65),(L,14),(W,62),(E,92),(K,30),(P,43)
test :: StorageMonad M
test = AVL.fromList [("Y",0),("X",0),("W",0),("V",0),("U",0),("S",0),("T",0),("P",0),("Q",0),("R",0),("X",0),("B",0),("D",0),("G",0),("I",0)]

main = do
    _ <- AVL.runEmptyCache test
    return ()