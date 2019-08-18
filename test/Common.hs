module Common
    ( module Common
    , module Lenses
    , module T
    ) where

import Control.Monad as T (unless, when)
import Control.Monad.Catch as T (catch)
import Control.Monad.IO.Class as T (MonadIO, liftIO)
import qualified Control.Lens as Lenses

import Data.Function (on)
import Data.Hashable (Hashable, hash)
import Data.List (nubBy, sortBy)
import Data.Ord (comparing)
import Data.Relation (Relates)
import Data.String (IsString)
import Data.Union (Member (..))

import Generic.Random

import GHC.Generics (Generic)

import Test.Hspec as T
import Test.QuickCheck as T
    ( Arbitrary (..)
    , Gen
    , Property
    , Testable
    , elements
    , forAll
    , ioProperty
    , property
    , (===)
    , (==>)
    )
import Test.QuickCheck.Instances as T ()

import qualified Data.Tree.AVL as AVL
import qualified Data.Tree.AVL.Store.Pure as Pure
import qualified Data.Tree.AVL.Store.Void as Void

type Layer = AVL.Rep IntHash StringName Int

instance Hashable a => AVL.ProvidesHash a IntHash where
    getHash = IntHash . hash

newtype IntHash = IntHash { getIntHash :: Int }
    deriving stock (Eq, Ord)
    deriving newtype Hashable

instance Show IntHash where
    show = take 8 . map convert . map (`mod` 16) . iterate (`div` 16) . abs . getIntHash
      where
        convert = ("0123456789ABCDEF" !!)

newtype StringName = StringName { getStringName :: String }
    deriving stock (Eq, Ord)
    deriving newtype Hashable
    deriving IsString via String

instance Show StringName where
    show = getStringName

instance Arbitrary StringName where
    arbitrary = do
        a <- elements ['B'.. 'Y']
        return (StringName [a])

data UKey
    = K1 StringName
    | K2 Bool
    deriving stock    (Eq, Ord, Show, Generic)
    deriving anyclass (Hashable)

instance Arbitrary UKey where
    arbitrary = genericArbitraryU

data UValue
    = V1 Int
    | V2 String
    deriving stock    (Eq, Ord, Show, Generic)
    deriving anyclass (Hashable)

instance Arbitrary UValue where
    arbitrary = genericArbitraryU

instance Relates StringName Int
instance Relates Bool       String

Lenses.makePrisms ''UKey
Lenses.makePrisms ''UValue

instance Member StringName UKey   where union = _K1
instance Member Bool       UKey   where union = _K2
instance Member Int        UValue where union = _V1
instance Member String     UValue where union = _V2

type StorageMonad = Void.Store IntHash StringName Int
type StorageMonad' = Pure.StoreT IntHash StringName Int StorageMonad

type M = AVL.Map IntHash StringName Int

type UStorageMonad = Void.Store IntHash UKey UValue
type UStorageMonad' = Pure.StoreT IntHash UKey UValue UStorageMonad

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

it'
    ::  ( Testable (f Property)
        , Testable  prop
        , Functor   f
        )
    =>  String
    ->  f (StorageMonad prop)
    ->  SpecWith ()
it' msg func =
    it msg $ property $ fmap (ioProperty . Void.runStoreT) func

it''
    ::  ( Testable   prop
        , Arbitrary  src
        , Show       src
        )
    =>  String
    ->  (src -> StorageMonad' prop)
    ->  SpecWith ()
it'' msg func =
    it msg $ property $ \src ->
        ioProperty $ Void.runStoreT $ do
            st <- Pure.newState
            Pure.runStoreT st (func src)

uit'
    ::  ( Testable (f Property)
        , Testable  prop
        , Functor   f
        )
    =>  String
    ->  f (UStorageMonad prop)
    ->  SpecWith ()
uit' msg func =
    it msg $ property $ fmap (ioProperty . Void.runStoreT) func

uit''
    ::  ( Testable   prop
        , Arbitrary  src
        , Show       src
        )
    =>  String
    ->  (src -> UStorageMonad' prop)
    ->  SpecWith ()
uit'' msg func =
    it msg $ property $ \src ->
        ioProperty $ Void.runStoreT $ do
            st <- Pure.newState
            Pure.runStoreT st (func src)

put :: (MonadIO m) => String -> m ()
put = liftIO . putStrLn
