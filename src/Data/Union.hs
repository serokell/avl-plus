
-- | The interface to open-union-like types to be used as keys and values.
--   This isn't some open union implementation, because if user doesn't
--   controls it, they can't implement some `Serialise` or whatnot without
--   much hassle. And no open union at the moment is trivially
--   `Serialisable`/`Hashable.`
--
--   Suggested way is to define `data UKey = K1 K1Ty | K2 ...` and use `lens`
--   to generate prisms.

module Data.Union
  ( Member(..)
  ) where

import Control.Lens (Prism')

-- |
class Member e u where
  union :: Prism' u e

instance Member e e where
  union = id
