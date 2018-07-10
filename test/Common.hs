
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

module Common (module Common, module Control.Lens, module T) where

import Control.Lens hiding (locus, elements, Empty)
import Control.Monad                        as T (when)
import Control.Monad.Catch                  as T (catch)
import Control.Monad.IO.Class               as T (liftIO)
import Control.Monad.Trans.Class            as T (lift)

import Data.Binary                               (Binary, encode, decodeOrFail)
import Data.ByteString.Lazy                      (toStrict, fromStrict)
import Data.Default                         as T (Default(def))
import Data.Foldable                             ()
import Data.Function                             (on)
import Data.Hashable                             (Hashable, hash)
import Data.HashMap.Strict                       (HashMap, fromList)
import Data.List                                 (sortBy, nubBy)
import Data.Ord                                  (comparing)
import Data.String                               (IsString(fromString))

import GHC.Generics                              (Generic)

import Test.Framework                       as T (Test, defaultMain, testGroup, TestName)
import Test.Framework.Providers.QuickCheck2 as T (testProperty)
import Test.QuickCheck                      as T ( Arbitrary (..), Gen, Property
                                                 , (===), (==>), elements
                                                 , ioProperty, Testable, forAll )
import Test.QuickCheck.Instances            as T ()

import qualified Data.Tree.AVL as AVL

instance (Binary x, Eq x, Show x) => AVL.Serialisable x where
    serialise   = toStrict . encode
    deserialise = decodeErrorToMaybe . decodeOrFail . fromStrict
      where
        decodeErrorToMaybe = either
            (\(_, _, err) -> Left err)
            (\(_, _, it)  -> Right it)

-- | Extensional equality combinator.
(.=.) :: (Eq b, Show b, Arbitrary a) => (a -> b) -> (a -> b) -> a -> Property
f .=. g = \a ->
  let fa = f a
      ga = g a

  in  fa === ga

infixr 5 .=.

--data InitialHash
--    = InitialHash { getInitialHash :: Layer }
--    | Default
--    deriving (Ord, Generic)

type Layer = AVL.MapLayer Int StringName Int Int

instance Binary Layer

deriving instance Ord Layer
instance Hashable Layer

instance Hashable b => Hashable (AVL.WithBounds b)
instance Binary   b => Binary   (AVL.WithBounds b)

instance Hashable StringName

instance Binary   AVL.Tilt
instance Hashable AVL.Tilt

--instance Show InitialHash where
--    show = \case
--        InitialHash m -> "#(" ++ show (m & AVL.mlHash .~ Default) ++ ")"
--        Default       -> "#"

--instance Default InitialHash where
--  def = Default

--instance Eq InitialHash where
--    x == y = show x == show y

--instance AVL.Hash InitialHash StringName Int where
--    hashOf = InitialHash

instance AVL.Hash Int StringName Int where
    hashOf tree = case tree of
        AVL.MLBranch _ mk ck t l r' -> hash (hash mk + hash ck + hash t + l + r')
        AVL.MLLeaf   _ k  v  n p    -> hash (hash k + hash v + hash n + hash p)
        AVL.MLEmpty  _              -> 0

-- newtype IntHash = IntHash { getIntHash :: Int }
--     deriving (Show, Eq, Arbitrary)
--
newtype StringName = StringName { getStringName :: String }
    deriving (Eq, Ord, Generic, Binary)

instance IsString StringName where
    fromString = StringName

instance Show StringName where
    show = getStringName

instance Arbitrary StringName where
    arbitrary = do
        a <- elements ['B'.. 'Y']
        return (StringName [a])

--instance Bounded StringName where
--    minBound = StringName "A"
--    maxBound = StringName "Z"

instance (Eq k, Hashable k) => Default (HashMap k v) where
    def = fromList []

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

type StorageMonad       = AVL.HashMapStore Int AVL.NullStore
type CachedStorageMonad = AVL.HashMapStore Int StorageMonad

type M = AVL.Map Int StringName Int

cachedProperty :: (Testable a, Arbitrary b, Show b) => TestName -> (b -> StorageMonad a) -> Test
cachedProperty msg prop =
    testProperty msg $
        forAll arbitrary $ \b ->
            ioProperty $ do
                (a, _st) <- AVL.runOnEmptyCache $ do
                    prop b

                return a

scanM :: Monad m => (a -> b -> m b) -> b -> [a] -> m [b]
scanM _      _     []       = return []
scanM action accum (x : xs) = do
    (accum :) <$> do
        accum' <- action x accum
        scanM action accum' xs

unique :: Eq a => [(a, b)] -> [(a, b)]
unique = nubBy  ((==) `on` fst)

uniqued :: Ord a => [(a, b)] -> [(a, b)]
uniqued = sortBy (comparing fst) . unique . reverse
