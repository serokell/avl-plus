{-|
    AVL+ tree.
-}
module Data.Tree.AVL
    ( -- * Required interfaces
      KVRetrieve (..)
    , KVStore (..)
    , KVMutate (..)

      -- * Constraints for most AVL+-related actions
    , Retrieves
    , Stores
    , Mutates

      -- * Some base constraints
    , Hash(hashOf)
    , Params
    , Base

      -- * Exceptions
    , NotFound (..)

      -- * Main type ('Map') and all other types
    , Map
    , MapLayer (..)
    , Tilt
    , WithBounds
    , Revision

      -- * Constructors/toList
    , empty
    , fromList
    , fromFoldable
    , toList

      -- * Root hash signature
    , rootHash

      -- * Operations over AVL+
    , insert
    , delete
    , deleteWithNoProof
    , lookup
    , fold
    , foldIf

      -- * Operations that change storage or request current state
    , save
    , overwrite
    , currentRoot
    , NoRootExists(..)
    , intialiseStorageIfNotAlready

      -- * Proof type, ways to construct and eliminate
    , Proof (..)
    , fullRehash
    , prune
    , checkProof

      -- * For debug
    , showMap
    ) where

import Prelude hiding (lookup)

import Data.Tree.AVL.Deletion
import Data.Tree.AVL.Insertion
import Data.Tree.AVL.Internal
import Data.Tree.AVL.Iteration
import Data.Tree.AVL.Lookup
import Data.Tree.AVL.Proof
import Data.Tree.AVL.Prune
import Data.Tree.AVL.Unsafe
