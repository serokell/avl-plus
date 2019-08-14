
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language TypeFamilies #-}

module Data.Union
  ( Member(..)
  ) where

class Member e u where
  inject  :: e -> u
  project :: u -> Maybe e

instance Member e e where
  inject  = id
  project = Just
