
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language TypeFamilies #-}

module Data.Union
  ( Union
  , inject
  , project
  , All
  , Dispatch
  , nothing
  , (\/)
  , match
  , Member
  , is
  ) where

import qualified Data.Array as Array
import Data.Array (Array, (!))
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))

import GHC.Exts (Any, Constraint)
import GHC.Generics (Generic)
import GHC.TypeLits (type (+), Nat, KnownNat, natVal)

import Unsafe.Coerce (unsafeCoerce)

data Union (fs :: [*]) = Case
  { uTag     :: Int
  , uPayload :: Any fs
  }
  deriving (Generic)

newtype Dispatch (fs :: [*]) r = Dispatch (Array Int (Any fs r))

match :: Dispatch fs r -> Union fs -> r
match (Dispatch handlers) (Case i payload) =
  handlers ! i `unsafeCoerce` payload

nothing :: Dispatch '[] r
nothing = Dispatch (array [])

infixr 1 \/

(\/) :: (f -> r) -> Dispatch fs r -> Dispatch (f : fs) r
handler \/ Dispatch rest =
  Dispatch
    $ unsafeCoerce
    $ array
    $ handler : unsafeCoerce (Array.elems rest)

array :: [a] -> Array Int a
array list = Array.listArray (0, length list - 1) list

type family All c (fs :: [*]) :: Constraint where
  All c '[]      = ()
  All c (f : fs) = (c f, All c fs)

type family Index f fs :: Nat where
  Index f (f : fs) = 0
  Index f (g : fs) = 1 + Index f fs

type Member f fs = KnownNat (Index f fs)

index :: forall f fs. Member f fs => Int
index = fromIntegral $ natVal (Proxy :: Proxy (Index f fs))

inject :: forall f fs. Member f fs => f -> Union fs
inject f = Case (index @f @fs) (unsafeCoerce f)

project :: forall f fs. Member f fs => Union fs -> Maybe f
project (Case i f)
  | index @f @fs == i = Just $ unsafeCoerce f
  | otherwise         = Nothing

is :: forall f fs. Member f fs => Union fs -> Bool
is = isJust . project @f @fs
