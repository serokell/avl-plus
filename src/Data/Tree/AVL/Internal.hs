
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE StrictData             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Tree.AVL.Internal where

import Control.Applicative ((<|>))
import Control.Lens (makeLenses, makePrisms, (&), (.~), (^.), (^?))

import Control.Monad (void, MonadPlus)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free

import Data.Binary
import Data.Default (Default (..))
import Data.Foldable (for_)
import Data.Typeable (Typeable)

import GHC.Generics hiding (to)

import Data.Tree.AVL.KVStoreMonad

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
    { _mlRevision :: Revision
    , _mlHash     :: h
    , _mlKey      :: k
    , _mlValue    :: v
    , _mlNextKey  :: k
    , _mlPrevKey  :: k
    }
  | MLEmpty
    { _mlRevision :: Revision
    , _mlHash     :: h
    }
  | MLPruned  -- | Has to contain all this data to act as a proper subtree.
    { _mlRevision  :: Revision
    , _mlHash      :: h
    , _mlTilt      :: Tilt
    , _mlMinKey    :: k
    , _mlCenterKey :: k
    }
    deriving (Eq, Functor, Foldable, Traversable, Generic, Binary)

type Map h k v m = FreeT (MapLayer h k v) m h

--instance (Show k, Show v, Show r) => Show (MapLayer h k v r) where
--  show = \case
--    MLBranch _ _ mk ck t l r -> "Branch " ++ show (mk, ck, t, l, r)
--    MLLeaf   _ _ k v n p     -> "Leaf " ++ show (k, v, p, n)
--    MLEmpty  _ _             -> "Empty"
--    MLPruned _ _ t mk ck     -> "Pruned " ++ show (t, mk, ck)

makeLenses ''MapLayer
makePrisms ''MapLayer

makeBranch :: Stores h k v m => Revision -> h -> k -> k -> Tilt -> Map h k v m -> Map h k v m -> Map h k v m
makeLeaf   :: Stores h k v m => Revision -> h -> k -> v -> k -> k -> Map h k v m
makeEmpty  :: Stores h k v m => Revision -> h -> Map h k v m
makePruned :: Stores h k v m => Revision -> h -> Tilt -> k -> k -> Map h k v m

makeBranch re hash mKey cKey t l r = hash `stored` MLBranch re hash mKey cKey t l r
makeLeaf   r  hash key  val  n p   = hash `stored` MLLeaf   r  hash key  val  n p
makeEmpty  r  hash                 = hash `stored` MLEmpty  r  hash
makePruned r  hash t mKey cKey     = hash `stored` MLPruned r  hash t mKey cKey

class    (Ord h, MonadIO m, MonadPlus m, Hash h k v, KVStoreMonad m h (MapLayer h k v h)) => Stores h k v m where
instance (Ord h, MonadIO m, MonadPlus m, Hash h k v, KVStoreMonad m h (MapLayer h k v h)) => Stores h k v m where

stored :: Stores h k v m => h -> MapLayer h k v (Map h k v m) -> Map h k v m
stored hash layer = do
    isolated <- traverse (lift . rootHash) layer
    lift $ store hash isolated
    FreeT $ return $ Free layer

rootHash :: Stores h k v m => Map h k v m -> m h
rootHash = pickAnd (^.mlHash)

class HasRevision m r | r -> m where
    revision    :: r -> m Revision
    setRevision :: Revision -> r -> r

-- | A revision lens for AVL tree.
instance Stores h k v m => HasRevision m (Map h k v m) where
    revision      = pickAnd (^.mlRevision)
    setRevision r = onTopNode (mlRevision .~ r)

--rootHash :: Getter (Map h k v) h
--rootHash = _Fix . to (^.mlHash)

pick :: Stores h k v m => Map h k v m -> m (MapLayer h k v (Map h k v m))
pick tree = do
    wand  <- runFreeT tree
    case wand of
      Pure key -> do
          actual <- retrieve key
          return (ref <$> actual)
      Free layer -> do
          return layer

pickAnd :: Stores h k v m => (MapLayer h k v (Map h k v m) -> a) -> Map h k v m -> m a
pickAnd f tree = f <$> pick tree

hide :: Stores h k v m => MapLayer h k v (Map h k v m) -> Map h k v m
hide layer = FreeT $ return $ Free layer

pickTree :: forall h k v m . Stores h k v m => h -> m (Map h k v m)
pickTree hash = do
    sublayer <- retrieve hash :: m (MapLayer h k v h)
    FreeT . return . Free <$> traverse pickTree sublayer

ref :: forall h k v m . Stores h k v m => h -> Map h k v m
ref h = FreeT $ return $ Pure h

onTopNode :: Stores h k v m => (MapLayer h k v (Map h k v m) -> MapLayer h k v (Map h k v m)) -> Map h k v m -> Map h k v m
onTopNode f tree = do
    layer <- lift $ pick tree
    hide (f layer)

save :: forall h k v m . Stores h k v m => Map h k v m -> m ()
save tree = do
    wand <- runFreeT tree

    case wand of
      Pure _     -> return ()
      Free layer -> do    
        let hash = layer^.mlHash
        
        void (retrieve hash :: m (MapLayer h k v h)) <|> do
            isolated <- traverse rootHash layer
            store hash isolated
            for_ layer save

        return ()

saveOne :: forall h k v m . Stores h k v m => Map h k v m -> m h
saveOne tree = do
    layer    <- pick tree
    rHash    <- rootHash tree
    isolated <- traverse rootHash layer
    store rHash isolated
    return rHash

--materialize
--    :: forall m h k v
--    .  Stores h k v m
--    => Map h k v m
--    -> m (MapLayer h k v h)
--materialize tree = do
--    wand  <- runFreeT tree
--    layer <- case wand of
--      Pure key -> do
--          layer <- retrieve key :: m (MapLayer h k v h)
--          traverse retrieveTree layer
--      --Free layer -> layer
--    return layer
--  where
--    retrieveTree :: h -> m (Map h k v m)
--    retrieveTree hash = do
--      layer <- retrieve hash :: m (MapLayer h k v h)
--      return $ FreeT $ Free <$> traverse retrieveTree layer

--materialize
--    :: forall m h k v
--    .  Stores h k v m
--    => Map h k v m
--    -> m (MapLayer h k v (Map h k v m))
--materialize tree = do
--    wand  <- runFreeT tree
--    layer <- case wand of
--      Pure key -> do
--          layer <- retrieve key :: m (MapLayer h k v h)
--          traverse retrieveTree layer
--      --Free layer -> layer
--    return layer
--  where
--    retrieveTree :: h -> m (Map h k v m)
--    retrieveTree hash = do
--      layer <- retrieve hash :: m (MapLayer h k v h)
--      return $ FreeT $ Free <$> traverse retrieveTree layer

--materializeAnd :: Stores h k v m => (MapLayer m h k v -> a) -> m a
--materializeAnd f tree = f <$> materialize tree

minKey :: (Bounded k, Stores h k v m) => Map h k v m -> m k
minKey = pickAnd $ \layer ->
    layer^?mlMinKey `orElse`
    layer^?mlKey `orElse`
    minBound

centerKey :: (Bounded k, Stores h k v m) => Map h k v m -> m k
centerKey = pickAnd $ \layer ->
    layer^?mlCenterKey `orElse`
    layer^?mlKey `orElse`
    minBound

tilt :: Stores h k v m => Map h k v m -> m Tilt
tilt = pickAnd $ \layer ->
    layer^?mlTilt `orElse` M

setLeft    :: Stores h k v m => Map h k v m -> Map h k v m -> Map h k v m
setRight   :: Stores h k v m => Map h k v m -> Map h k v m -> Map h k v m
setNextKey :: Stores h k v m => k           -> Map h k v m -> Map h k v m
setPrevKey :: Stores h k v m => k           -> Map h k v m -> Map h k v m
setValue   :: Stores h k v m => v           -> Map h k v m -> Map h k v m

setLeft    left  = onTopNode (mlLeft    .~ left)
setRight   right = onTopNode (mlRight   .~ right)
setNextKey k     = onTopNode (mlNextKey .~ k)
setPrevKey k     = onTopNode (mlPrevKey .~ k)
setValue   v     = onTopNode (mlValue   .~ v)

----setRight   :: Setter' (Map h k v) (Map h k v)
----setValue   :: Setter' (Map h k v) v
----setNextKey :: Setter' (Map h k v) k
----setPrevKey :: Setter' (Map h k v) k

----setLeft    = _Fix.mlLeft
----setRight   = _Fix.mlRight
----setValue   = _Fix.mlValue
----setNextKey = _Fix.mlNextKey
----setPrevKey = _Fix.mlPrevKey

----branching :: Getter (Map h k v) (Maybe (Branching h k v))
----branching = to $ \case
----  Branch _ _ _ _ t l r -> Just (Branching l r t)
----  _                    -> Nothing

----terminal :: Getter (Map h k v) (Maybe (Terminal h k v))
----terminal = to $ \case
----  Leaf _ _ k v n p -> Just (Terminal k v n p)
----  _                -> Nothing

----vacuous :: Getter (Map h k v) (Maybe Vacuous)
----vacuous = to $ \case
----  Empty _ _ -> Just (Vacuous)
----  _         -> Nothing

-- | Recalculate 'rootHash' of the node.
--   Does nothing on 'Pruned' node.
rehash :: Stores h k v m => Map h k v m -> Map h k v m
rehash tree = do
    layer <- lift $ pick tree
    case layer of
      MLPruned {} ->
        tree
      
      _ -> do
        isolated <- lift $ traverse rootHash layer
        hide $ layer & mlHash .~ hashOf (isolated & mlHash .~ def)

-- | Interface for calculating hash of the 'Map' node.
class
    (Ord k, Show k, Show h, Typeable h, Show v, Bounded k, Eq h, Default h)
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
pattern Node :: Revision -> Tilt -> a -> a -> MapLayer h k v a
pattern Node re d l r <- MLBranch re _ _ _ d l r

empty :: Stores h k v m => Map h k v m
empty = rehash $ hide $ MLEmpty 0 def

-- | Smart constructor for 'Pruned' node.
--   Turns node into 'Pruned' one.
--   Does nothing on already 'Pruned' node.
pruned :: Stores h k v m => Map h k v m -> Map h k v m
pruned tree = do
  layer <- lift $ pick tree
  case layer of
    MLPruned {} -> tree
    _other    -> hide $ MLPruned
      (layer^.mlRevision)
      (layer^.mlHash)
      (layer^?mlTilt      
        `orElse` M)
      (layer^?mlMinKey    `orElse` minBound)
      (layer^?mlCenterKey `orElse` minBound)

-- | Construct a branch from 2 subtrees.
--   Recalculates 'rootHash', 'minKey' and 'centerKey'.
branch :: Stores h k v m => Revision -> Tilt -> Map h k v m -> Map h k v m -> Map h k v m
branch r tilt0 left right = do
    left0  <- lift $ pick left
    right0 <- lift $ pick right
    rehash $ hide $ MLBranch
        r
        def
        ((left0^?mlMinKey `orElse` minBound) `min` (right0^?mlMinKey `orElse` minBound))
        (right0^?mlMinKey `orElse` minBound)
        tilt0
        left
        right

leaf :: Stores h k v m => Revision -> k -> v -> k -> k -> Map h k v m
leaf r k v p n = rehash $ hide $ MLLeaf r def k v n p

lessThanCenterKey :: Stores h k v m => k -> Map h k v m -> m Bool
lessThanCenterKey key0 tree = do
    layer <- pick tree
    return $ key0 < (layer^?mlCenterKey `orElse` minBound)

toList :: Stores h k v m => Map h k v m -> m [(k, v)]
toList = go
  where
    go tree = do
      layer <- pick tree
      case layer of
        MLLeaf   {_mlKey , _mlValue} -> return [(_mlKey, _mlValue)]
        MLBranch {_mlLeft, _mlRight} -> (++) <$> go _mlLeft <*> go _mlRight
        _                            -> return []

size :: Stores h k v m => Map h k v m -> m Integer
size = go
  where
    go tree = do
        layer <- pick tree
        case layer of
          MLEmpty  {}                  -> return 0
          MLBranch {_mlLeft, _mlRight} -> (+) <$> go _mlLeft <*> go _mlRight 
          _                            -> return 1

-- | For testing purposes. Finds lengths of all paths to the leaves.
pathLengths :: Stores h k v m => Map h k v m -> m [Int]
pathLengths = go
  where
    go tree = do
      layer <- pick tree
      case layer of
        MLLeaf   {}                  -> return [0]
        MLBranch {_mlLeft, _mlRight} -> do
          lefts  <- go _mlLeft
          rights <- go _mlRight
          return $ map (+ 1) $ lefts ++ rights
        _                            -> return []
