
{-# language FunctionalDependencies #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language TemplateHaskell #-}
{-# language NamedFieldPuns #-}
{-# language PatternSynonyms #-}
{-# language StrictData #-}

module Data.Tree.AVL.Internal where

import Control.Lens hiding (Empty)

import Data.Monoid
import Data.Tree.Pretty as T
import Data.Tree        as T

import Debug.Trace      as Debug

-- | I decided to isolate all the hash-y things from tree topology
--   for the sake of it not getting in the way.
data Payload h k = Payload
    { _pRootHash  :: h
    , _pMinKey    :: k
    , _pCenterKey :: k
    } deriving (Show)

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
        { _aptRevision :: Revision
        , _aptPayload  :: Payload h k
        , _aptTilt     :: Tilt
        , _aptLeft     :: Map h k v
        , _aptRight    :: Map h k v }
    | Leaf
        { _aptRevision :: Revision
        , _aptHash     :: h
        , _aptKey      :: k
        , _aptValue    :: v
        , _aptNextKey  :: k
        , _aptPrevKey  :: k }
    | Empty
        { _aptRevision :: Revision }

type Revision = Integer

data Side = L | R deriving (Eq, Show)

other L = R
other R = L

makeLenses ''Map
makeLenses ''Payload

class Combined h where
    combine  :: (h, Side, Tilt, h) -> h
    emptyOne :: h

class
    (Ord k, Show k, Show h, Show v, Bounded k, Eq h, Combined h)
      =>
    Hash h k v | k v -> h
  where
    hashOf :: (k, v, k, k) -> h

squeese :: Payload h k -> h
squeese = (^.pRootHash)

class HasRevision s where
    revision :: Lens' s Revision

instance HasRevision (Map h k v) where
    revision = aptRevision

tilt :: Lens' (Map h k v) Tilt
tilt = lens get (\m t -> m & aptTilt .~ t)
  where
    get tree = tree^?aptTilt `orElse` M

infix 1 `orElse`
Just x `orElse` _ = x
_      `orElse` x = x

rootHash :: Hash h k v => Getter (Map h k v) h
rootHash = to $ \tree -> case tree of
  Branch {} -> tree^?!aptPayload.pRootHash
  Leaf   {} -> tree^?!aptHash
  Empty  {} -> emptyOne

minKey :: Hash h k v => Getter (Map h k v) k
minKey = to $ \tree -> case tree of
  Branch {} -> tree^?!aptPayload.pMinKey
  Leaf   {} -> tree^?!aptKey
  Empty  {} -> minBound

centerKey :: Hash h k v => Getter (Map h k v) k
centerKey = to $ \tree -> case tree of
  Branch {} -> tree^?!aptPayload.pCenterKey
  Leaf   {} -> tree^?!aptKey
  Empty  {} -> minBound

isBadlyBalanced :: Map h k v -> Bool
isBadlyBalanced = \case
  branch @ Branch {} ->
    let left    = branch^?!aptLeft
        right   = branch^?!aptRight
        dHeight = branch^.tilt
    in or
        [ isBadlyBalanced left
        , isBadlyBalanced right
        ,  height right - height left
        /= fromEnum dHeight - fromEnum M
        ]

  _ -> False

height :: Map h k v -> Int
-- | Find the length of the longest path to the leaf.
height tree = case tree of
  Leaf   {} -> 1
  Empty  {} -> 0
  Branch {} ->
    1 + max (tree^?!aptLeft.to height) (tree^?!aptRight.to height)

instance Hash h k v => Show (Map h k v) where
    -- show tree = "fromList " ++ show (toList tree)

    -- Uncomment the following to get structured printing:

    -- show = \case
    --   thunk @ (Branch rev pld d l r) -> mempty
    --     <> "{" <> show l
    --     <> " " <> show d
    --     <> ":" <> show (height r - height l)
    --     <> "/" <> show rev
    --     <> " " <> show r
    --     <> "}"
    --
    --   Leaf { aptKey, aptNextKey, aptPrevKey, aptValue, aptRevision } ->
    --     ""
    --     <> "{" <> show aptKey
    --     <> ":" <> show aptRevision
    --     <> "}"
    --
    --
    --   Empty  {} -> "-"
    show = T.drawTree . pp
      where
        pp = \case
          b @ (Branch rev pld d l r) ->
            T.Node (show d <> ":" <> show (height r - height l) <> ":" <> show rev)
              [ pp l
              , pp r
              ]

          Leaf   { _aptRevision, _aptKey, _aptHash, _aptPrevKey, _aptNextKey } ->
              T.Node (show _aptPrevKey <> " > " <> show _aptKey <> " < " <> show _aptNextKey <> " = " <> show _aptHash) []
          Empty  {}          -> T.Node "EMPTY" []

pattern Node :: Tilt -> Map h k v -> Map h k v -> Map h k v
-- | For clarity of rebalance procedure.
pattern Node d l r <- Branch _ _ d l r

branch :: Hash h k v => Revision -> Tilt -> Map h k v -> Map h k v -> Map h k v
-- | Construct a branch from 2 subtrees. Recalculates hash.
branch rev tilt left right = Branch
  { _aptRevision = rev
  , _aptPayload  = joinPayloads tilt left right
  , _aptLeft     = left
  , _aptRight    = right
  , _aptTilt     = tilt
  }

joinPayloads :: Hash h k v => Tilt -> Map h k v -> Map h k v -> Payload h k
-- | Construct a payload from 2 subtrees' payloads.
joinPayloads tilt left right = Payload
  { _pRootHash   = combine (left^.rootHash, L, tilt, right^.rootHash)
  , _pMinKey     = (left^.minKey) `min` (right^.minKey)
  , _pCenterKey  = right^.minKey
  }

leaf :: Hash h k v => Revision -> k -> v -> k -> k -> Map h k v
leaf r k v p n = Leaf
    { _aptRevision = r
    , _aptHash     = hashOf (k, v, p, n)
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

      Empty {} -> tree

lessThanCenterKey :: Hash h k v => k -> Map h k v -> Bool
lessThanCenterKey key map = key < (map^.centerKey)

empty = Empty { _aptRevision = 0 }

toList :: Map h k v -> [(k, v)]
toList tree = case tree of
  Empty  {}                    -> []
  Leaf   {} -> [(tree^?!aptKey, tree^?!aptValue)]
  Branch {} -> toList (tree^?!aptLeft) <> toList (tree^?!aptRight)

instance Foldable (Map h k) where
    foldMap f = go
      where
        go tree = case tree of
          Empty  {} -> mempty
          Leaf   {} -> f (tree^?!aptValue)
          Branch {} -> go (tree^?!aptLeft) <> go (tree^?!aptRight)

pathLengths :: Map h k v -> [Int]
-- | For testing purposes. Finds lengths of all paths to the leaves.
pathLengths tree = case tree of
  Empty  {} -> []
  Leaf   {} -> [0]
  Branch {} ->
    map (+ 1) (pathLengths (tree^?!aptLeft) ++ pathLengths (tree^?!aptRight))
