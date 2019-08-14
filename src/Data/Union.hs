
-- | The interface to open-union-like types to be used as keys and values.

module Data.Union
  ( Member(..)
  ) where

import Control.Lens

-- |
class Member e u where
  union :: Prism' u e

instance Member e e where
  union = id
