
{-# language FunctionalDependencies #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language TemplateHaskell #-}
{-# language NamedFieldPuns #-}
{-# language PatternSynonyms #-}
{-# language StrictData #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language RankNTypes #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric              #-}
{-# language StandaloneDeriving #-}
{-# language DeriveAnyClass #-}
{-# language UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Tree.AVL.Internal where

import Control.Lens ( (&), (^.), (.~), (^?)
                    , to
                    , makeLenses, makePrisms
                    , Getter, Setter', Lens'
                    )
import Data.Binary
import Data.Default (Default(..))
import Data.Fix     (Fix(..))

import GHC.Generics hiding (to)

-- | Current semantics: a "unique name" for a node.
--   TODO: verify that no order comparison used, use `rootHash` instead.
type Revision = Integer

data Side = L | R deriving (Eq, Show, Generic, Binary)

-- | "Tilt" is a difference in branch heights.
--   We have only +2/-2 as our limits for the rebalance, lets make enum.
--   1) This will be compiled to number comparison;
--   2) This will die fast in case some part of the algorithm tries to inc R2.
data Tilt
    = L2  -- UnbalancedLeft
    | L1  -- BalancedLeft
    | M   -- Balanced
    | R1  -- BalancedRight
    | R2  -- UnbalancedRight
    deriving (Eq, Ord, Enum, Show, Generic, Binary)

-- | Representation of AVL+ tree with data in leaves.

-- | One layer of AVL Tree structure.
--   It is build that way to guarantee 'hashOf' implementation to touch only
--   current level and be syncronised with any changes in tree structure.
data MapLayer h k v self
  = MLBranch
    { _mlRevision  :: Revision
    , _mlHash      :: h
    , _mlMinKey    :: k
    , _mlCenterKey :: k     -- Could be removed at cost of 1 additional descent.
    , _mlTilt      :: Tilt
    , _mlLeft      :: self
    , _mlRight     :: self
    }
  | MLLeaf
    { _mlRevision  :: Revision
    , _mlHash      :: h
    , _mlKey       :: k
    , _mlValue     :: v
    , _mlNextKey   :: k
    , _mlPrevKey   :: k
    }
  | MLEmpty
    { _mlRevision  :: Revision
    , _mlHash      :: h
    }
  | MLPruned  -- | Has to contain all this data to act as a proper subtree.
    { _mlRevision  :: Revision
    , _mlHash      :: h
    , _mlTilt      :: Tilt
    , _mlMinKey    :: k
    , _mlCenterKey :: k
    }
    deriving (Eq, Functor, Generic, Binary)

type Map h k v = Fix (MapLayer h k v)

deriving instance Binary (f (Fix f)) => Binary (Fix f)

instance (Show k, Show v, Show r) => Show (MapLayer h k v r) where
  show = \case
    MLBranch _ _ mk ck t l r -> "Branch " ++ show (mk, ck, t, l, r)
    MLLeaf   _ _ k v n p     -> "Leaf " ++ show (k, v, p, n)
    MLEmpty  _ _             -> "Empty"
    MLPruned _ _ t mk ck     -> "Pruned " ++ show (t, mk, ck)

-- | A view on tree node that can branch.
data Branching h k v = Branching
  { _left  :: Map h k v
  , _right :: Map h k v
  , _tilt  :: Tilt
  }

-- | A view on tree node that has actual data.
data Terminal h k v = Terminal
  { _key     :: k
  , _value   :: v
  , _nextKey :: k
  , _prevKey :: k
  }

-- | A view on an empty node.
data Vacuous = Vacuous

makeLenses ''Terminal
makeLenses ''Branching
makeLenses ''MapLayer
makePrisms ''MapLayer
makePrisms ''Fix

-- | Patterns to hide the 'Fix' plumbing between levels.
pattern Branch :: Revision -> h -> k -> k -> Tilt -> Map h k v -> Map h k v -> Map h k v
pattern Leaf   :: Revision -> h -> k -> v -> k -> k -> Map h k v
pattern Empty  :: Revision -> h -> Map h k v
pattern Pruned :: Revision -> h -> Tilt -> k -> k -> Map h k v

pattern Branch re hash mKey cKey t l r = Fix (MLBranch re hash mKey cKey t l r)
pattern Leaf   r  hash key  val  n p   = Fix (MLLeaf   r  hash key  val  n p)
pattern Empty  r  hash                 = Fix (MLEmpty  r  hash)
pattern Pruned r  hash t mKey cKey     = Fix (MLPruned r  hash t mKey cKey)

class HasRevision r where
    revision :: Lens' r Revision

-- | A revision lens for AVL tree.
instance HasRevision (Map h k v) where
    revision = _Fix . mlRevision

rootHash :: Getter (Map h k v) h
rootHash = _Fix . to (^.mlHash)

minKey :: Bounded k => Getter (Map h k v) k
minKey = _Fix . to (\tree ->
    tree^?mlMinKey `orElse`
    tree^?mlKey `orElse`
    minBound)

centerKey :: Bounded k => Getter (Map h k v) k
centerKey = _Fix . to (\tree ->
    tree^?mlCenterKey `orElse`
    tree^?mlKey `orElse`
    minBound)

setLeft    :: Setter' (Map h k v) (Map h k v)
setRight   :: Setter' (Map h k v) (Map h k v)
setValue   :: Setter' (Map h k v) v
setNextKey :: Setter' (Map h k v) k
setPrevKey :: Setter' (Map h k v) k

setLeft    = _Fix.mlLeft
setRight   = _Fix.mlRight
setValue   = _Fix.mlValue
setNextKey = _Fix.mlNextKey
setPrevKey = _Fix.mlPrevKey

branching :: Getter (Map h k v) (Maybe (Branching h k v))
branching = to $ \case
  Branch _ _ _ _ t l r -> Just (Branching l r t)
  _                    -> Nothing

terminal :: Getter (Map h k v) (Maybe (Terminal h k v))
terminal = to $ \case
  Leaf _ _ k v n p -> Just (Terminal k v n p)
  _                -> Nothing

vacuous :: Getter (Map h k v) (Maybe Vacuous)
vacuous = to $ \case
  Empty _ _ -> Just (Vacuous)
  _         -> Nothing

-- | Recalculate 'rootHash' of the node.
--   Does nothing on 'Pruned' node.
rehash :: Hash h k v => Map h k v -> Map h k v
rehash node @ (Fix tree) = case tree of
  MLPruned {} -> node
  _           -> Fix $ tree & mlHash .~ hashOf cleaned
  where
    cleaned = fmap (^.rootHash) $ tree & mlHash .~ def

-- | Interface for calculating hash of the 'Map' node.
class
    (Ord k, Show k, Show h, Show v, Bounded k, Eq h, Default h)
      =>
    Hash h k v
  where
    hashOf :: MapLayer h k v h -> h

another :: Side -> Side
another L = R
another R = L

infixr 1 `orElse`
orElse :: Maybe a -> a -> a
Just x `orElse` _ = x
_      `orElse` x = x


-- | For clarity of rebalance procedure.
pattern Node :: Revision -> Tilt -> Map h k v -> Map h k v -> Map h k v
pattern Node re d l r <- Branch re _ _ _ d l r

empty :: Hash h k v => Map h k v
empty = rehash $ Empty 0 def

-- | Smart constructor for 'Pruned' node.
--   Turns node into 'Pruned' one.
--   Does nothing on already 'Pruned' node.
pruned :: Hash h k v => Map h k v -> Map h k v
pruned tree = case tree of
  Pruned {} -> tree
  _other    -> Pruned
    (tree^.revision)
    (tree^.rootHash)
    (tree^?_Fix.mlTilt `orElse` M)
    (tree^.minKey)
    (tree^.centerKey)

-- | Construct a branch from 2 subtrees.
--   Recalculates 'rootHash', 'minKey' and 'centerKey'.
branch :: Hash h k v => Revision -> Tilt -> Map h k v -> Map h k v -> Map h k v
branch r tilt0 left0 right0 = rehash $ Branch
    r
    def
    ((left0^.minKey) `min` (right0^.minKey))
    (right0^.minKey)
    tilt0
    left0
    right0

leaf :: Hash h k v => Revision -> k -> v -> k -> k -> Map h k v
leaf r k v p n = rehash $ Leaf r def k v n p

lessThanCenterKey :: Hash h k v => k -> Map h k v -> Bool
lessThanCenterKey key0 tree = key0 < (tree^.centerKey)

toList :: Map h k v -> [(k, v)]
toList tree
  | Just term <- tree^.terminal
    = [(term^.key, term^.value)]

  | Just fork <- tree^.branching
    = toList (fork^.left) ++ toList (fork^.right)

  | Just Vacuous <- tree^.vacuous
    = []

  | otherwise
    = []

size :: Map h k v -> Integer
size = go
  where
    go tree
      | Just Vacuous <- tree^.vacuous   = 0
      | Just _       <- tree^.terminal  = 1
      | Just fork    <- tree^.branching = go (fork^.left) + go (fork^.right)
      | otherwise                       = 1

-- | For testing purposes. Finds lengths of all paths to the leaves.
pathLengths :: Map h k v -> [Int]
pathLengths tree = case tree of
  Leaf   {} -> [0]
  Pruned {} -> error "pathLengths: Pruned"
  _ | Just fork <- tree^.branching ->
    map (+ 1) (pathLengths (fork^.left) ++ pathLengths (fork^.right))
  _         -> []
