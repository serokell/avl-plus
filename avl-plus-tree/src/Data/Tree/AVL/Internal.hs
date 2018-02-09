
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

module Data.Tree.AVL.Internal where

import Control.Lens hiding (Empty)

import Data.Monoid
import Data.Proxy

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

-- | Implementation of AVL+ tree with data in leaves.
data Map h k v
    = Branch
        { _aptRevision  :: Revision
        , _aptHash      :: h
        , _aptMinKey    :: k
        , _aptCenterKey :: k
        , _aptTilt      :: Tilt
        , _aptLeft      :: Map h k v
        , _aptRight     :: Map h k v }
    | Leaf
        { _aptRevision  :: Revision
        , _aptHash      :: h
        , _aptKey       :: k
        , _aptValue     :: v
        , _aptNextKey   :: k
        , _aptPrevKey   :: k }
    | Empty
        { _aptRevision  :: Revision
        , _aptHash      :: h
        }
    | Pruned
        { _aptRevision  :: Revision
        , _aptHash      :: h
        , _aptMinKey    :: k
        , _aptCenterKey :: k
        }
    deriving Show

class
    ( Ord k
    , Show k
    , Show h
    , Show v
    , Bounded k
    , Eq h
    , Monoid h
    )
      =>
    Hash h k v
  where
    ofBranch :: (Revision, k, k, Tilt, h, h, Proxy v) -> h
    ofLeaf   :: (Revision, k, v, k, k) -> h

type Revision = Integer

data Side = L | R deriving (Eq, Show)

another :: Side -> Side
another L = R
another R = L

makeLenses ''Map

class HasRevision s where
    revision :: Lens' s Revision

instance HasRevision (Map h k v) where
    revision = aptRevision

tilt :: Lens' (Map h k v) Tilt
tilt = lens get (\m t -> m & aptTilt .~ t)
  where
    get tree = tree^?aptTilt `orElse` M

infix 1 `orElse`
orElse :: Maybe a -> a -> a
Just x `orElse` _ = x
_      `orElse` x = x

rootHash :: Hash h k v => Getter (Map h k v) h
rootHash = aptHash

minKey :: Hash h k v => Getter (Map h k v) k
minKey = to $ \tree -> case tree of
  Empty {} -> minBound
  _other   -> tree^?!aptKey

centerKey :: Hash h k v => Getter (Map h k v) k
centerKey = to $ \tree -> case tree of
  Empty {} -> minBound
  _other   -> tree^?!aptKey

-- isBadlyBalanced :: Map h k v -> Bool
-- isBadlyBalanced = \case
--   branch @ Branch {} ->
--     let left    = branch^?!aptLeft
--         right   = branch^?!aptRight
--         dHeight = branch^.tilt
--     in or
--         [ isBadlyBalanced left
--         , isBadlyBalanced right
--         ,  height right - height left
--         /= fromEnum dHeight - fromEnum M
--         ]

--   _ -> False

-- height :: Map h k v -> Int
-- -- | Find the length of the longest path to the leaf.
-- height tree = case tree of
--   Leaf   {} -> 1
--   Empty  {} -> 0
--   Branch {} ->
--     1 + max (tree^?!aptLeft.to height) (tree^?!aptRight.to height)

-- instance Hash h k v => Show (Map h k v) where
--     -- show tree = "fromList " ++ show (toList tree)

--     -- Uncomment the following to get structured printing:

--     -- show = \case
--     --   thunk @ (Branch rev pld d l r) -> mempty
--     --     <> "{" <> show l
--     --     <> " " <> show d
--     --     <> ":" <> show (height r - height l)
--     --     <> "/" <> show rev
--     --     <> " " <> show r
--     --     <> "}"
--     --
--     --   Leaf { aptKey, aptNextKey, aptPrevKey, aptValue, aptRevision } ->
--     --     ""
--     --     <> "{" <> show aptKey
--     --     <> ":" <> show aptRevision
--     --     <> "}"
--     --
--     --
--     --   Empty  {} -> "-"
--     show = T.drawTree . pp
--       where
--         pp = \case
--           Branch rev _pld d l r ->
--             T.Node (show d <> ":" {- <> show (height r - height l) -} <> ":" <> show rev)
--               [ pp l
--               , pp r
--               ]

--           Leaf   { _aptRevision, _aptKey, _aptHash, _aptPrevKey, _aptNextKey } ->
--               T.Node (show _aptPrevKey <> " > " <> show _aptKey <> " < " <> show _aptNextKey <> " = " <> show _aptHash) []
--           Empty  {}          -> T.Node "EMPTY"  []
--           Pruned {}          -> T.Node "PRUNED" []

pattern Node :: Tilt -> Map h k v -> Map h k v -> Map h k v
-- | For clarity of rebalance procedure.
pattern Node d l r <- Branch _ _ _ _ d l r

empty :: Hash h k v => Map h k v
empty = Empty { _aptRevision = 0, _aptHash = mempty }

pruneNode :: Hash h k v => Map h k v -> Map h k v
pruneNode tree = case tree of
  Pruned {} -> tree
  _other    -> Pruned
    (tree^.revision)
    (tree^.rootHash)
    (tree^.minKey)
    (tree^.centerKey)

branch
    :: forall h k v
    .  Hash   h k v
    => Revision
    -> Tilt
    -> Map h k v
    -> Map h k v
    -> Map h k v
-- | Construct a branch from 2 subtrees. Recalculates hash.
branch rev tilt0 left right = Branch
  { _aptRevision  = rev
  , _aptHash      = ofBranch (rev, mKey, cKey, tilt0, lHash, rHash, Proxy :: Proxy v)
  , _aptMinKey    = mKey
  , _aptCenterKey = cKey
  , _aptLeft      = left
  , _aptRight     = right
  , _aptTilt      = tilt0
  }
  where
    mKey  = (left^.minKey) `min` (right^.minKey)
    cKey  = right^.minKey
    lHash = left^.rootHash
    rHash = right^.rootHash

leaf :: Hash h k v => Revision -> k -> v -> k -> k -> Map h k v
leaf r k v p n = Leaf
    { _aptRevision = r
    , _aptHash     = ofLeaf (r, k, v, p, n)
    , _aptKey      = k
    , _aptValue    = v
    , _aptNextKey  = n
    , _aptPrevKey  = p
    }

rehash :: Hash h k v => Map h k v -> Map h k v
rehash tree =
    case tree of
      Branch {} -> branch
        (tree^.revision)
        (tree^.tilt)
        (tree^?!aptLeft)
        (tree^?!aptRight)

      Leaf {} -> leaf
        (tree^.revision)
        (tree^?!aptKey)
        (tree^?!aptValue)
        (tree^?!aptPrevKey)
        (tree^?!aptNextKey)

      Empty  {} -> tree
      Pruned {} -> tree

lessThanCenterKey :: Hash h k v => k -> Map h k v -> Bool
lessThanCenterKey key tree = key < (tree^.centerKey)

toList :: Map h k v -> [(k, v)]
toList tree = case tree of
  Empty  {}                    -> []
  Leaf   {_aptKey,  _aptValue} -> [(_aptKey, _aptValue)]
  Branch {_aptLeft, _aptRight} -> toList _aptLeft <> toList _aptRight
  Pruned {}                    -> error "toList: Pruned"

instance Foldable (Map h k) where
    foldMap f = go
      where
        go tree = case tree of
          Empty  {} -> mempty
          Leaf   {} -> f (tree^?!aptValue)
          Branch {} -> go (tree^?!aptLeft) <> go (tree^?!aptRight)
          Pruned {} -> error "foldMap: Pruned"

pathLengths :: Map h k v -> [Int]
-- | For testing purposes. Finds lengths of all paths to the leaves.
pathLengths tree = case tree of
  Empty  {} -> []
  Leaf   {} -> [0]
  Pruned {} -> error "pathLengths: Pruned"
  Branch {} ->
    map (+ 1) (pathLengths (tree^?!aptLeft) ++ pathLengths (tree^?!aptRight))

olderThan :: Map h k v -> Revision -> Bool
olderThan tree rev = tree^.revision > rev
