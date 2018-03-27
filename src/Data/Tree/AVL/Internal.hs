
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

--instance (Show k, Show v, Show r) => Show (MapLayer h k v r) where
--  show = \case
--    MLBranch _ _ mk ck t l r -> "Branch " ++ show (mk, ck, t, l, r)
--    MLLeaf   _ _ k v n p     -> "Leaf " ++ show (k, v, p, n)
--    MLEmpty  _ _             -> "Empty"
--    MLPruned _ _ t mk ck     -> "Pruned " ++ show (t, mk, ck)

makeLenses ''MapLayer
makePrisms ''MapLayer

showMap :: (Show h, Show k, Show v) => Map h k v -> String
showMap = drawTree . asTree 
  where
    asTree = \case
      Free (MLBranch r _ mk ck t l r') -> Tree.Node ("-< " ++ show (r, t)) [asTree r', asTree l]
      Free (MLLeaf   r _ k  v  n p)    -> Tree.Node ("<3- "   ++ show (r, k)) []
      Free (MLEmpty  r _)              -> Tree.Node "--" []
      --Free (MLPruned r _ t m c)        -> Tree.Node "++" []
      Pure  h                          -> Tree.Node ("Ref " ++ show h) []

    unfuck
        = replace "├" "+"
        . replace "└" "\\"
        . replace "│" "|"
        . replace "╴" "-"
        . replace "╴" "-"

instance (Show h, Show k, Show v, Show self) => Show (MapLayer h k v self) where
    show = \case
      MLBranch r h mk ck t l r' -> "Branch" ++ show (r, h, t, l, r')
      MLLeaf   r h k  v  n p    -> "Leaf" ++ show (r, h, k)
      MLEmpty  r h              -> "--" ++ show (r, h)
      --MLPruned r _ t m c        -> "??"

makeBranch :: Revision -> h -> k -> k -> Tilt -> Map h k v -> Map h k v -> Map h k v
makeLeaf   :: Revision -> h -> k -> v -> k -> k -> Map h k v
makeEmpty  :: Revision -> h -> Map h k v
--makePruned :: Revision -> h -> Tilt -> k -> k -> Map h k v

makeBranch re hash mKey cKey t l r = Free $ MLBranch re hash mKey cKey t l r
makeLeaf   r  hash key  val  n p   = Free $ MLLeaf   r  hash key  val  n p
makeEmpty  r  hash                 = Free $ MLEmpty  r  hash
--makePruned r  hash t mKey cKey     = Free $ MLPruned r  hash t mKey cKey

class    (Ord h, Typeable k, MonadIO m, Hash h k v, KVStoreMonad m h (MapLayer h k v h)) => Stores h k v m where
instance (Ord h, Typeable k, MonadIO m, Hash h k v, KVStoreMonad m h (MapLayer h k v h)) => Stores h k v m where

rootHash :: Stores h k v m => Map h k v -> m h
rootHash = \case
  Pure h     -> return h
  Free layer -> return (layer^.mlHash)

revision :: Stores h k v m => Map h k v -> m Revision
revision = openAnd (^.mlRevision)

setRevision :: Stores h k v m => Revision -> Map h k v -> m (Map h k v)
setRevision r = onTopNode (mlRevision .~ r)

isolate :: Stores h k v m => Map h k v -> m (Map h k v)
isolate tree = do
    layer <- open tree
    isolated <- traverse (fmap Pure . rootHash) layer
    return (close isolated)

--rootHash :: Getter (Map h k v) h
--rootHash = _Fix . to (^.mlHash)

open :: Stores h k v m => Map h k v -> m (MapLayer h k v (Map h k v))
open = \case
  Pure key -> do
    actual <- retrieve key
    return (ref <$> actual)
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
  Pure  _            -> return ()
  --Free (MLPruned {}) -> return ()
  
  Free  layer -> do    
    let hash = layer^.mlHash
    
    void (retrieve hash :: m (MapLayer h k v h))
      `catch` \(NotFound (_ :: h)) -> do
        isolated <- traverse rootHash layer
        store hash isolated
        for_ layer save

    return ()

saveOne :: forall h k v m . Stores h k v m => Map h k v -> m h
saveOne tree = do
    layer    <- open tree
    isolated <- traverse rootHash layer
    let rHash = layer^.mlHash
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

--isolate :: Stores h k v m => Map h k v -> m (MapLayer h k v h)
--isolate tree = do
--    layer <- open tree
--    traverse rootHash layer

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
rehash :: Stores h k v m => Map h k v -> m (Map h k v)
rehash tree = do
    layer    <- open tree
    isolated <- traverse rootHash layer
    let tree = close $ layer & mlHash .~ hashOf (isolated & mlHash .~ def)
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
pattern Node :: Revision -> Tilt -> a -> a -> MapLayer h k v a
pattern Node re d l r <- MLBranch re _ _ _ d l r

empty :: Stores h k v m => m (Map h k v)
empty = rehash $ close $ MLEmpty 0 def

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
branch :: Stores h k v m => Revision -> Tilt -> Map h k v -> Map h k v -> m (Map h k v)
branch r tilt0 left right = do
    left0        <- open left
    right0       <- open right
    [minL, minR] <- traverse minKey [left, right]
    rehash $ close $ MLBranch r def (min minL minR) minR tilt0 left right

leaf :: Stores h k v m => Revision -> k -> v -> k -> k -> m (Map h k v)
leaf r k v p n = do
    rehash $ close $ MLLeaf r def k v n p

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
