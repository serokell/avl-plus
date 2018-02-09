
{-# language FunctionalDependencies #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language TemplateHaskell #-}
{-# language NamedFieldPuns #-}
{-# language PatternSynonyms #-}
-- {-# language StrictData #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language RankNTypes #-}
{-# language DeriveFunctor #-}

module Data.Tree.AVL.Internal where

import Control.Lens hiding (Empty)

-- import Data.Monoid
-- import Data.Proxy
import Data.Default
import Data.Fix

-- import Debug.Trace      as Debug

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
    deriving (Eq, Ord, Enum, Show)

-- | Representation of AVL+ tree with data in leaves.

type Map h k v = Fix (MapLayer h k v)

data MapLayer h k v self
  = MLBranch
    { _mlRevision  :: Revision
    , _mlHash      :: h
    , _mlMinKey    :: k
    , _mlCenterKey :: k
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
  | MLPruned
    { _mlRevision  :: Revision
    , _mlHash      :: h
    , _mlMinKey    :: k
    , _mlCenterKey :: k
    }
    deriving (Functor)

instance (Show k, Show v, Show r) => Show (MapLayer h k v r) where
  show = \case
    MLBranch _ _ mk ck t l r -> show (mk, ck, t, l, r)
    MLLeaf   _ _ k v n p     -> show (k, v, p, n)
    MLEmpty  _ _             -> show ()
    MLPruned _ _ mk ck       -> show (mk, ck)

type Revision = Integer

data Side = L | R deriving (Eq, Show)

data Branching h k v = Branching
  { _left  :: Map h k v
  , _right :: Map h k v
  , _tilt  :: Tilt
  }

data Terminal h k v = Terminal
  { _key     :: k
  , _value   :: v
  , _nextKey :: k
  , _prevKey :: k
  }

data Vacuous = Vacuous

makeLenses ''Terminal
makeLenses ''Branching
makeLenses ''MapLayer
makePrisms ''MapLayer
makePrisms ''Fix

pattern Branch :: Revision -> h -> k -> k -> Tilt -> Map h k v -> Map h k v -> Map h k v
pattern Leaf   :: Revision -> h -> k -> v -> k -> k -> Map h k v
pattern Empty  :: Revision -> h -> Map h k v
pattern Pruned :: Revision -> h -> k -> k -> Map h k v

pattern Branch re hash mKey cKey t l r = Fix (MLBranch re hash mKey cKey t l r)
pattern Leaf   r  hash key  val  n p   = Fix (MLLeaf   r  hash key  val  n p)
pattern Empty  r  hash                 = Fix (MLEmpty  r  hash)
pattern Pruned r  hash mKey cKey       = Fix (MLPruned r  hash mKey cKey)

class HasRevision r where
  revision :: Lens' r Revision

instance HasRevision (Map h k v) where
  revision = _Fix . mlRevision

rootHash :: Getter (Map h k v) h
rootHash = _Fix . to (^.mlHash)

minKey :: Bounded k => Getter (Map h k v) k
minKey = _Fix . to (\tree -> tree^?mlMinKey `orElse` minBound)

centerKey :: Bounded k => Getter (Map h k v) k
centerKey = _Fix . to (\tree -> tree^?mlCenterKey `orElse` minBound)

setLeft :: Setter' (Map h k v) (Map h k v)
setLeft = _Fix.mlLeft

setRight :: Setter' (Map h k v) (Map h k v)
setRight = _Fix.mlRight

setValue :: Setter' (Map h k v) v
setValue = _Fix.mlValue

setNextKey :: Setter' (Map h k v) k
setNextKey = _Fix.mlNextKey

setPrevKey :: Setter' (Map h k v) k
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

rehash :: Hash h k v => Map h k v -> Map h k v
rehash (Fix tree) = Fix $ tree & mlHash .~ hashOf cleaned
  where
    cleaned = tree & mlHash .~ () & fmap (^.rootHash)

class
    ( Ord k
    , Show k
    , Show h
    , Show v
    , Bounded k
    , Eq h
    , Default h
    )
      =>
    Hash h k v
  where
    hashOf :: MapLayer () k v h -> h

another :: Side -> Side
another L = R
another R = L

infix 1 `orElse`
orElse :: Maybe a -> a -> a
Just x `orElse` _ = x
_      `orElse` x = x


pattern Node :: Tilt -> Map h k v -> Map h k v -> Map h k v
-- | For clarity of rebalance procedure.
pattern Node d l r <- Branch _ _ _ _ d l r

empty :: Hash h k v => Map h k v
empty = Empty 0 def

pruned :: Hash h k v => Map h k v -> Map h k v
pruned tree = case tree of
  Pruned {} -> tree
  _other    -> Pruned
    (tree^.revision)
    (tree^.rootHash)
    (tree^.minKey)
    (tree^.centerKey)

branch :: Hash h k v => Revision -> Tilt -> Map h k v -> Map h k v -> Map h k v
-- | Construct a branch from 2 subtrees. Recalculates hash.
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
    = error "toList: Pruned"

size :: Map h k v -> Integer
size = go
  where
    go tree
      | Just Vacuous <- tree^.vacuous   = 0
      | Just _       <- tree^.terminal  = 1
      | Just fork    <- tree^.branching = go (fork^.left) + go (fork^.right)
      | otherwise                       = error "foldMap: Pruned"

pathLengths :: Map h k v -> [Int]
-- | For testing purposes. Finds lengths of all paths to the leaves.
pathLengths tree = case tree of
  Leaf   {} -> [0]
  Pruned {} -> error "pathLengths: Pruned"
  _ | Just fork <- tree^.branching ->
    map (+ 1) (pathLengths (fork^.left) ++ pathLengths (fork^.right))
  _         -> []

-- olderThan :: Map h k v -> Revision -> Bool
-- olderThan tree rev = tree^.revision > rev
