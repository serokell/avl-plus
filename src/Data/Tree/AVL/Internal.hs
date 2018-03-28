
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

import Control.Lens (makeLenses, makePrisms, (&), (.~), (^.), (^?))

import Control.Monad (void, when)
import Control.Monad.Catch (catch)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Free

import Data.Binary
import Data.String.Utils (replace)
import Data.Default (Default (..))
import Data.Hashable (Hashable)
import Data.Foldable (for_)
import Data.Typeable (Typeable)
import Data.Tree as Tree (Tree(Node), drawTree)
--import Data.Tree.View (showTree)

import GHC.Generics (Generic)

import Data.Tree.AVL.KVStoreMonad

import qualified Debug.Trace as Debug

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
    deriving (Eq, Ord, Show, Enum, Generic, Binary)

--instance Show Tilt where
--  show = \case
--    L2 -> "<<"
--    L1 -> "<"
--    M  -> "."
--    R1 -> ">"
--    R2 -> ">>"

-- | Representation of AVL+ tree with data in leaves.

-- | One layer of AVL Tree structure.
--   It is build that way to guarantee 'hashOf' implementation to touch only
--   current level and be syncronised with any changes in tree structure.
data MapLayer h k v self
  = MLBranch
    { _mlHash      :: h
    , _mlMinKey    :: k
    , _mlCenterKey :: k     -- Could be removed at cost of 1 additional descent.
    , _mlTilt      :: Tilt
    , _mlLeft      :: self
    , _mlRight     :: self
    }
  | MLLeaf
    { _mlHash     :: h
    , _mlKey      :: k
    , _mlValue    :: v
    , _mlNextKey  :: k
    , _mlPrevKey  :: k
    }
  | MLEmpty
    { _mlHash     :: h
    }
  -- | MLPruned  -- | Has to contain all this data to act as a proper subtree.
  --  { _mlRevision  :: Revision
  --  , _mlHash      :: h
  --  , _mlTilt      :: Tilt
  --  , _mlMinKey    :: k
  --  , _mlCenterKey :: k
  --  }
    deriving (Eq, Functor, Foldable, Traversable, Generic, Binary)

deriving instance                                      Generic (Free t a)
deriving instance (Binary (t (Free t a)), Binary a) => Binary  (Free t a)

type Map h k v = Free (MapLayer h k v) h

makeLenses ''MapLayer
makePrisms ''MapLayer

showMap :: (Show h, Show k, Show v) => Map h k v -> String
showMap = drawTree . asTree
  where
    asTree = \case
      Free (MLBranch _ mk ck t l r') -> Tree.Node ("-< " ++ show (t)) [asTree r', asTree l]
      Free (MLLeaf   _ k  v  n p)    -> Tree.Node ("<3- "   ++ show (k)) []
      Free (MLEmpty  _)              -> Tree.Node "--" []
      Pure  h                          -> Tree.Node ("Ref " ++ show h) []

instance (Show h, Show k, Show v, Show self) => Show (MapLayer h k v self) where
    show = \case
      MLBranch h mk ck t l r' -> "Branch" ++ show (h, t, l, r')
      MLLeaf   h k  v  n p    -> "Leaf" ++ show (h, k)
      MLEmpty  h              -> "--" ++ show (h)
      --MLPruned r _ t m c        -> "??"

makeBranch :: h -> k -> k -> Tilt -> Map h k v -> Map h k v -> Map h k v
makeLeaf   :: h -> k -> v -> k -> k -> Map h k v
makeEmpty  :: h -> Map h k v

makeBranch hash mKey cKey t l r = Free $ MLBranch hash mKey cKey t l r
makeLeaf   hash key  val  n p   = Free $ MLLeaf   hash key  val  n p
makeEmpty  hash                 = Free $ MLEmpty  hash

class    (Ord h, Typeable k, MonadIO m, Hash h k v, KVStoreMonad m h (MapLayer h k v h)) => Stores h k v m where
instance (Ord h, Typeable k, MonadIO m, Hash h k v, KVStoreMonad m h (MapLayer h k v h)) => Stores h k v m where

rootHash :: Map h k v -> h
rootHash = \case
  Pure h     -> h
  Free layer -> layer^.mlHash

isolate :: Stores h k v m => Map h k v -> m (Map h k v)
isolate = openAnd $ close . fmap (Pure . rootHash)

open :: Stores h k v m => Map h k v -> m (MapLayer h k v (Map h k v))
open = \case
  Pure key -> do
    actual <- retrieve key
    return (Pure <$> actual)
  Free layer -> do
    return layer

openAnd :: Stores h k v m => (MapLayer h k v (Map h k v) -> a) -> Map h k v -> m a
openAnd f tree = f <$> open tree

close :: MapLayer h k v (Map h k v) -> Map h k v
close = Free

--pickTree :: forall h k v m . Stores h k v m => h -> m (Map h k v m)
--pickTree hash = do
--    sublayer <- retrieve hash :: m (MapLayer h k v h)
--    FreeT . return . Free <$> traverse pickTree sublayer

ref :: h -> Map h k v
ref = Pure

onTopNode :: Stores h k v m => (MapLayer h k v (Map h k v) -> MapLayer h k v (Map h k v)) -> Map h k v -> m (Map h k v)
onTopNode f tree = do
    layer <- open tree
    rehash $ close (f layer)

save :: forall h k v m . Stores h k v m => Map h k v -> m ()
save = \case
  Pure _     -> return ()
  Free layer -> do
    let hash = layer^.mlHash

    void (retrieve hash :: m (MapLayer h k v h))
      `catch` \(NotFound (_ :: h)) -> do
        store hash (rootHash <$> layer)
        for_ layer save

    return ()

saveOne :: forall h k v m . Stores h k v m => Map h k v -> m h
saveOne tree = do
    layer    <- open tree
    let rHash = layer^.mlHash

    store rHash (rootHash <$> layer)
    return rHash

minKey :: (Bounded k, Stores h k v m) => Map h k v -> m k
minKey = openAnd $ \layer ->
    layer^?mlMinKey `orElse`
    layer^?mlKey `orElse`
    minBound

centerKey :: (Bounded k, Stores h k v m) => Map h k v -> m k
centerKey = openAnd $ \layer ->
    layer^?mlCenterKey `orElse`
    layer^?mlKey `orElse`
    minBound

tilt :: Stores h k v m => Map h k v -> m Tilt
tilt = openAnd $ \layer ->
    layer^?mlTilt `orElse` M

setLeft    :: Stores h k v m => Map h k v -> Map h k v -> m (Map h k v)
setRight   :: Stores h k v m => Map h k v -> Map h k v -> m (Map h k v)
setNextKey :: Stores h k v m => k         -> Map h k v -> m (Map h k v)
setPrevKey :: Stores h k v m => k         -> Map h k v -> m (Map h k v)
setValue   :: Stores h k v m => v         -> Map h k v -> m (Map h k v)

setLeft    left  = onTopNode (mlLeft    .~ left)
setRight   right = onTopNode (mlRight   .~ right)
setNextKey k     = onTopNode (mlNextKey .~ k)
setPrevKey k     = onTopNode (mlPrevKey .~ k)
setValue   v     = onTopNode (mlValue   .~ v)

-- | Recalculate 'rootHash' of the node.
--   Does nothing on 'Pruned' node.
rehash :: Stores h k v m => Map h k v -> m (Map h k v)
rehash tree = do
    layer <- open tree

    let isolated = rootHash <$> layer
    let cleaned  = isolated & mlHash .~ def
    let tree     = close $ layer & mlHash .~ hashOf cleaned

    saveOne tree
    return tree

-- | Interface for calculating hash of the 'Map' node.
class
    (Ord k, Show k, Show h, Hashable h, Typeable h, Show v, Bounded k, Eq h, Default h)
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
pattern Node :: h -> Tilt -> a -> a -> MapLayer h k v a
pattern Node h d l r <- MLBranch h _ _ d l r

empty :: Stores h k v m => m (Map h k v)
empty = rehash $ close $ MLEmpty def

-- | Smart constructor for 'Pruned' node.
--   Turns node into 'Pruned' one.
--   Does nothing on already 'Pruned' node.
--pruned :: Stores h k v m => Map h k v -> m (Map h k v)
--pruned tree = do
--    open tree >>= \case
--      --MLPruned {} -> do
--      --  return tree
--      layer -> do
--        return $ close $ MLPruned
--            (layer^.mlRevision)
--            (layer^.mlHash)
--            (layer^?mlTilt      `orElse` M)
--            (layer^?mlMinKey    `orElse` minBound)
--            (layer^?mlCenterKey `orElse` minBound)

-- | Construct a branch from 2 subtrees.
--   Recalculates 'rootHash', 'minKey' and 'centerKey'.
branch :: Stores h k v m => Tilt -> Map h k v -> Map h k v -> m (Map h k v)
branch tilt0 left right = do
    left0        <- open left
    right0       <- open right
    [minL, minR] <- traverse minKey [left, right]
    rehash $ close $ MLBranch def (min minL minR) minR tilt0 left right

leaf :: Stores h k v m => k -> v -> k -> k -> m (Map h k v)
leaf k v p n = do
    rehash $ close $ MLLeaf def k v n p

lessThanCenterKey :: Stores h k v m => k -> Map h k v -> m Bool
lessThanCenterKey key0 = openAnd $ \layer ->
    key0 < (layer^?mlCenterKey `orElse` minBound)

toList :: Stores h k v m => Map h k v -> m [(k, v)]
toList = go
  where
    go tree = do
      open tree >>= \case
        MLLeaf   {_mlKey , _mlValue} -> return [(_mlKey, _mlValue)]
        MLBranch {_mlLeft, _mlRight} -> (++) <$> go _mlLeft <*> go _mlRight
        _                            -> return []

size :: Stores h k v m => Map h k v -> m Integer
size = go
  where
    go tree = do
        open tree >>= \case
          MLEmpty  {}                  -> return 0
          MLBranch {_mlLeft, _mlRight} -> (+) <$> go _mlLeft <*> go _mlRight
          _                            -> return 1

-- | For testing purposes. Finds lengths of all paths to the leaves.
pathLengths :: Stores h k v m => Map h k v -> m [Int]
pathLengths = go
  where
    go tree = do
      open tree >>= \case
        MLLeaf   {}                  -> return [0]
        MLBranch {_mlLeft, _mlRight} -> do
          lefts  <- go _mlLeft
          rights <- go _mlRight
          return $ map (+ 1) $ lefts ++ rights
        _                            -> return []

height :: Stores h k v m => Map h k v -> m Integer
height = go
  where
    go tree = do
        open tree >>= \case
          MLEmpty  {}                  -> return 0
          MLBranch {_mlLeft, _mlRight} -> (\x y -> 1 + max x y) <$> go _mlLeft <*> go _mlRight
          _                            -> return 1

isBalancedToTheLeaves :: forall h k v m . Stores h k v m => Map h k v -> m Bool
isBalancedToTheLeaves = go
  where
    go :: Map h k v -> m Bool
    go tree = do
        open tree >>= \case
          MLBranch {_mlLeft, _mlRight, _mlTilt} -> do
            leftH  <- height _mlLeft
            rightH <- height _mlRight
            let
              localBalance = case _mlTilt of
                L2 -> leftH - rightH == 2
                L1 -> leftH - rightH == 1
                M  -> leftH - rightH == 0
                R1 -> leftH - rightH == -1
                R2 -> leftH - rightH == -2

            leftGood  <- go _mlLeft
            rightGood <- go _mlRight

            return $ localBalance && leftGood && rightGood

          _ ->
            return True
