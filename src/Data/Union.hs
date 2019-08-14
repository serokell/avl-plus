
-- | The interface to open-union-like types to be used as keys and values.

module Data.Union
  ( Member(..)
  ) where

-- |
class Member e u where
  inject  :: e -> u
  project :: u -> Maybe e

instance Member e e where
  inject  = id
  project = Just
