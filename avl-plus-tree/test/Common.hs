
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Common (module Common, module Control.Lens) where

import Control.Lens hiding (locus, elements, Empty)

import           Data.Bits (xor)
import           Data.Foldable
import           Data.Function (on)
import           Data.List (sortBy, nubBy)
import           Data.Monoid
import           Data.Ord (comparing)

import qualified Data.Tree.AVL            as AVLPlus

import           Test.Framework             (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck            (Arbitrary (..), Gen, Property,
                                             (===), (==>), elements)
import           Test.QuickCheck.Instances  ()

-- | Extensional equality combinator.
(.=.) :: (Eq b, Show b, Arbitrary a) => (a -> b) -> (a -> b) -> a -> Property
f .=. g = \a ->
  let fa = f a
      ga = g a

  in  fa === ga

infixr 5 .=.

data InitialHash
    = HashOf (StringName, Int, StringName, StringName)
    | Combine (InitialHash, AVLPlus.Side, AVLPlus.Tilt, InitialHash)
    | EmptyOne

instance Eq InitialHash where
    x == y = show x == show y

instance Show InitialHash where
    show = \case
      HashOf   (k, _, p, n)        -> show (p, k, n)
      Combine (l, AVLPlus.L, t, r) -> "(" <> show l <> " # " <> show t <> " # " <> show r <> ")"
      Combine (l, AVLPlus.R, t, r) -> "(" <> show r <> " # " <> show t <> " # " <> show l <> ")"
      EmptyOne                     -> "0"

instance AVLPlus.Combined InitialHash where
    emptyOne = EmptyOne
    combine  = Combine

instance AVLPlus.Hash InitialHash StringName Int where
    hashOf = HashOf

-- newtype IntHash = IntHash { getIntHash :: Int }
--     deriving (Show, Eq, Arbitrary)
--
newtype StringName = StringName { getStringName :: String }
    deriving (Eq, Ord)

instance Show StringName where
    show = getStringName

instance Arbitrary StringName where
    arbitrary = do
        a <- elements ['B'.. 'Y']
        return (StringName [a])

instance Bounded StringName where
    minBound = StringName "A"
    maxBound = StringName "Z"

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

instance
    ( AVLPlus.Hash h k v
    , Arbitrary k
    , Arbitrary v
    , Show h
    )
      =>
    Arbitrary (AVLPlus.Map h k v)
  where
    arbitrary = AVLPlus.fromList <$> arbitrary

-- Requirement of QuickCheck
instance Show (a -> b) where
    show _ = "<function>"

type M = AVLPlus.Map InitialHash StringName Int
