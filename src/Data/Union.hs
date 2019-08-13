
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language TypeFamilies #-}

module Data.Union where

import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))

import GHC.Exts (Any{-, Constraint-})
import GHC.Generics (Generic)
import GHC.TypeLits (type (+), type (-), Nat, KnownNat, natVal)

import Unsafe.Coerce (unsafeCoerce)

data Union (fs :: [*])
  = Case Int (Any fs)
  deriving (Generic)

-- type family All c (fs :: [*]) :: Constraint where
--   All c '[]      = ()
--   All c (f : fs) = (c f, All c fs)

-- instance
--   ( Show f
--   , Show (Union fs)
--   , KnownNat (Len fs)
--   )
--   => Show (Union (f : fs))
--   where
--     show = either show show . split

-- instance Show (Union '[]) where
--   show _ = "Impossible"

type family At fs n where
  At (f : _)  0 = f
  At (_ : fs) n = At fs (n - 1)

type family Index f fs :: Nat where
  Index f (f : fs) = 0
  Index f (g : fs) = 1 + Index f fs

type family Len fs :: Nat where
  Len '[]       = 0
  Len  (f : fs) = 1 + Len fs

type Member f fs = (KnownNat (Index f fs), KnownNat (Len fs))

offset :: forall f fs. Member f fs => Int
offset = fromIntegral $ natVal (Proxy :: Proxy (Index f fs))

len :: forall fs. KnownNat (Len fs) => Int
len = fromIntegral $ natVal (Proxy :: Proxy (Len fs))

index :: forall f fs. Member f fs => Int
index = len @fs - offset @f @fs - 1

inject :: forall f fs. Member f fs => f -> Union fs
inject f = Case (index @f @fs) (unsafeCoerce f)

project :: forall f fs. Member f fs => Union fs -> Maybe f
project (Case i f)
  | index @f @fs == i = Just $ unsafeCoerce f
  | otherwise         = Nothing

is :: forall f fs. Member f fs => Union fs -> Bool
is = isJust . project @f @fs

-- split :: forall f fs. KnownNat (Len fs) => Union (f : fs) -> Either f (Union fs)
-- split    (Case n f) | n == len @fs = Left  (unsafeCoerce f)
-- split it@(Case n f)                = Right (unsafeCoerce it)