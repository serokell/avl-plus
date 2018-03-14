
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Common (module Common, module Control.Lens, module T) where

import Control.Lens hiding (locus, elements, Empty)

import Data.Bits                                 (xor)
import Data.Default                              (Default(def))
import Data.Foldable                             ()
import Data.Function                             (on)
import Data.List                                 (sortBy, nubBy)
import Data.Monoid                               ((<>))
import Data.Ord                                  (comparing)

import Test.Framework                       as T (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 as T (testProperty)
import Test.QuickCheck                      as T ( Arbitrary (..), Gen, Property
                                                 , (===), (==>), elements )
import Test.QuickCheck.Instances            as T ()

import qualified Data.Tree.AVL as AVL

-- | Extensional equality combinator.
(.=.) :: (Eq b, Show b, Arbitrary a) => (a -> b) -> (a -> b) -> a -> Property
f .=. g = \a ->
  let fa = f a
      ga = g a

  in  fa === ga

infixr 5 .=.

data InitialHash
  = InitialHash { getInitialHash :: AVL.MapLayer InitialHash StringName Int InitialHash }
  | Default

instance Show InitialHash where
    show = \case
        InitialHash m -> "#(" ++ show m ++ ")"
        Default       -> "#DEFAULT"

instance Default InitialHash where
  def = Default

instance Eq InitialHash where
    x == y = show x == show y

instance AVL.Hash InitialHash StringName Int where
    hashOf tree = case tree of
        AVL.MLPruned {} -> tree^.AVL.mlHash
        other           -> InitialHash tree

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
    ( AVL.Hash h k v
    , Arbitrary k
    , Arbitrary v
    , Show h
    )
      =>
    Arbitrary (AVL.Map h k v)
  where
    arbitrary = AVL.fromList <$> arbitrary

-- Requirement of QuickCheck
instance Show (a -> b) where
    show _ = "<function>"

type M = AVL.Map InitialHash StringName Int
