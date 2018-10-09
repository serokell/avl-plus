{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE StrictData             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Tree.AVL.Internal where

import           Control.Exception   (Exception)
import           Control.Lens        (makeLenses, (&), (.~), (^.), (^?))
import           Control.Monad       (void)
import           Control.Monad.Catch (MonadCatch, catch)
import           Control.Monad.Free  (Free (Free, Pure))

import           Data.ByteString     (ByteString)
import           Data.Foldable       (for_)
--import           Data.Hashable       (Hashable)
import           Data.Set            (Set)
import           Data.Tree           as Tree
import           Data.Typeable       (Typeable)

import           GHC.Generics        (Generic)

import           Text.Show.Deriving  (deriveShow1)

import qualified Data.Set            as Set (fromList)

-------------------------------------------------------------------------------
-- Datatypes
-------------------------------------------------------------------------------

data Side
    = L
    | R
    deriving (Eq, Show, Generic)

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
    deriving (Eq, Ord, Show, Enum, Generic)

data WithBounds b
    = Top
    | Plain b
    | Bottom
    deriving (Eq, Show, Generic)

instance Bounded (WithBounds b) where
    minBound = Bottom
    maxBound = Top

instance Ord b => Ord (WithBounds b) where
    Top     <= Top     = True
    Top     <= _       = False
    _       <= Top     = True

    Plain a <= Plain b = a <= b

    Bottom  <= Bottom  = True
    Bottom  <= _       = True
    _       <= Bottom  = False

fromWithBounds :: WithBounds b -> Maybe b
fromWithBounds (Plain b) = Just b
fromWithBounds  _        = Nothing

unsafeFromWithBounds :: WithBounds b -> b
unsafeFromWithBounds (Plain b) = b
unsafeFromWithBounds  _        = error "unsafeFromWithBounds: 'Top' or 'Bottom' values cannot be converted"

-- | Representation of AVL+ tree with data in leaves.

-- | One layer of AVL Tree structure.
--   It is build that way to guarantee 'hashOf' implementation to touch only
--   current level and be syncronised with any changes in tree structure.
data MapLayer h k v self
  = MLBranch
    { _mlHash      :: h
    , _mlMinKey    :: WithBounds k
    , _mlCenterKey :: WithBounds k
    , _mlTilt      :: Tilt
    , _mlLeft      :: self
    , _mlRight     :: self
    }
  | MLLeaf
    { _mlHash    :: h
    , _mlKey     :: WithBounds k
    , _mlValue   :: v
    , _mlNextKey :: WithBounds k
    , _mlPrevKey :: WithBounds k
    }
  | MLEmpty
    { _mlHash      :: h
    }
    deriving (Eq, Functor, Foldable, Traversable, Generic)

deriveShow1 ''MapLayer

type Map h k v = Free (MapLayer h k v) h

#if !MIN_VERSION_free(5,0,2)
deriving instance Generic (Free t a)
#endif

makeLenses ''MapLayer

-------------------------------------------------------------------------------
-- Typeclasses
-------------------------------------------------------------------------------

-- | Class, representing an ability for type to be [de]serialised.
class Serialisable a where
    serialise   :: a -> ByteString
    deserialise :: ByteString -> Either String a

-- | Class, representing DB layer, capable of storing 'isolate'd nodes.
--   The 'store' is an idempotent operation.
class (Serialisable k, Monad m) => KVStoreMonad k m where
    retrieve :: Serialisable v => k -> m v
    store    :: Serialisable v => k -> v -> m ()

-- | Interface for calculating hash of the 'Map' node.
class Hash h k v where
    hashOf :: MapLayer h k v h -> h
    -- ^ Take hash of the 'MapLayer'
    defHash :: h
    -- ^ Default hash, may be any.

-- | Exception to be thrown when node with given hashkey is missing.
data NotFound k = NotFound k
    deriving (Show, Typeable)

-- | Excepton to be thrown if deserialisation is failed.
data DeserialisationError = DeserialisationError String
    deriving (Show, Typeable)

instance Exception DeserialisationError

instance (Show k, Typeable k) => Exception (NotFound k)

-- | Umbrella class to grab all the required capabilities for tree to operate (and be debugged!).
type Stores h k v m =
    ( Ord h, Show h, Typeable h
    , Ord k, Show k, Typeable k
    , Hash h k v
    , KVStoreMonad h m
    , Serialisable (MapLayer h k v h)
    , Serialisable h
    , MonadCatch m
    )

-------------------------------------------------------------------------------
-- Methods
-------------------------------------------------------------------------------

-- | Debug preview of the tree.
showMap :: (Show h, Show k, Show v) => Map h k v -> String
showMap = drawTree . asTree
 where
   asTree = \case
     Free (MLBranch _ _mk _ck t  l r) -> Tree.Node ("-< "  ++ show (t)) [asTree r, asTree l]
     Free (MLLeaf   _  k  _v _n _p)   -> Tree.Node ("<3- " ++ show (k)) []
     Free (MLEmpty  _)                -> Tree.Node ("--")               []
     Pure  h                          -> Tree.Node ("Ref " ++ show h)   []

instance (Show h, Show k, Show v, Show self) => Show (MapLayer h k v self) where
    show = \case
      MLBranch h _mk _ck  t  l r -> "Branch" ++ show (h, t, l, r)
      MLLeaf   h  k  _v  _n _p   -> "Leaf"   ++ show (h, k)
      MLEmpty  h                 -> "--"     ++ show (h)

-- | Calculate hash outside of 'rehash'.
hashOf' :: forall h k v a. Hash h k v => MapLayer a k v h -> h
hashOf' ml = hashOf (ml & mlHash .~ (defHash @h @k @v))

-- | Get hash of the root node for the tree.
rootHash :: Map h k v -> h
rootHash = \case
  Pure h     -> h
  Free layer -> layer^.mlHash

walkDFS
  :: forall h k v m b res
  .  Stores h k v m
  =>  ( b
      , MapLayer h k v h -> b -> b
      , b -> res
      )
  -> Map h k v
  -> m res
walkDFS (start, add, finish) tree = finish <$> go start tree
  where
    -- We're doing it in DSF matter to save space.
    go :: b -> Map h k v -> m b
    go acc mapping = do
        open mapping >>= \point -> do
            let point' = rootHash <$> point
            case point of
              MLBranch { _mlLeft = l, _mlRight = r } -> do
                acc' <- go (add point' acc) l
                go acc' r

              MLLeaf {} -> do
                return $ add point' acc

              MLEmpty {} -> do
                return acc

-- | Fold the tree in the order of keys acsending.
fold :: Stores h k v m => (b, (k, v) -> b -> b, b -> res) -> Map h k v -> m res
fold (start, add, finish) = walkDFS (start, collectKVAnd add, finish)
  where
    -- We're doing it in DSF matter to save space.
    collectKVAnd act = \case
        MLLeaf { _mlKey = k, _mlValue = v } -> act (unsafeFromWithBounds k, v)
        _other                              -> id

-- | Get set of all node hashes from a tree.
allRootHashes :: Stores h k v m => Map h k v -> m (Set h)
allRootHashes = walkDFS ([], addHash, Set.fromList)
  where
    addHash layer = (_mlHash layer :)

-- | Replace direct children with references on them.
isolate :: Stores h k v m => Map h k v -> m (Map h k v)
isolate = openAnd $ close . fmap (Pure . rootHash)

-- | Unwrap the tree, possibly materializing its top node from the database.
open :: forall h k v m . (Hash h k v, Stores h k v m) => Map h k v -> m (MapLayer h k v (Map h k v))
open = \case
  Pure key ->
    let eH = emptyHash @h @k @v
     in if key == eH
           then pure $ MLEmpty eH
           else fmap Pure <$> retrieve key
  Free layer -> do
    return layer

-- | Unwrap the tree and apply a function.
openAnd :: Stores h k v m => (MapLayer h k v (Map h k v) -> a) -> Map h k v -> m a
openAnd f tree = f <$> open tree

-- | Unwrap the tree and apply a monadic action.
openAndM :: Stores h k v m => (MapLayer h k v (Map h k v) -> m a) -> Map h k v -> m a
openAndM f tree = f =<< open tree

-- | Wrap node layer into the tree.
close :: MapLayer h k v (Map h k v) -> Map h k v
close = Free

-- | Turn hash into unmaterialized tree.
ref :: h -> Map h k v
ref = Pure

-- | The analog to `fmap` :: (layer -> layer) -> tree -> m tree.
-- | The tree is rehashed after mapping.
onTopNode :: Stores h k v m => (MapLayer h k v (Map h k v) -> MapLayer h k v (Map h k v)) -> Map h k v -> m (Map h k v)
onTopNode f tree = do
    layer <- open tree
    rehash $ close (f layer)

-- | Recursively store all the materialized nodes in the database.
save :: forall h k v m . (Stores h k v m, MonadCatch m) => Map h k v -> m ()
save = go
  where
    go = \case
      Pure _     -> return ()
      Free layer -> do
        let hash = layer^.mlHash

        void (retrieve hash :: m (MapLayer h k v h))
          `catch` \(NotFound (_ :: h)) -> do
            store hash (rootHash <$> layer)
            for_ layer go

        return ()

-- | Store only top node of given tree.
saveOne :: forall h k v m . Stores h k v m => Map h k v -> m ()
saveOne tree = do
    layer    <- open tree
    let rHash = layer^.mlHash

    store rHash (rootHash <$> layer)

-- | Returns minimal key contained in a tree (or a 'minbound' if empty).
minKey :: Stores h k v m => Map h k v -> m (WithBounds k)
minKey = openAnd $ \layer ->
    layer^?mlMinKey `orElse`
    layer^?mlKey `orElse`
    minBound

centerKey :: Stores h k v m => Map h k v -> m (WithBounds k)
centerKey = openAnd $ \layer ->
    layer^?mlCenterKey `orElse`
    layer^?mlKey `orElse`
    minBound

-- | Returns balance parameter of the tree.
tilt :: Stores h k v m => Map h k v -> m Tilt
tilt = openAnd $ \layer ->
    layer^?mlTilt `orElse` M

setLeft    :: Stores h k v m => Map h k v    -> Map h k v -> m (Map h k v)
setRight   :: Stores h k v m => Map h k v    -> Map h k v -> m (Map h k v)
setNextKey :: Stores h k v m => WithBounds k -> Map h k v -> m (Map h k v)
setPrevKey :: Stores h k v m => WithBounds k -> Map h k v -> m (Map h k v)
setValue   :: Stores h k v m => v            -> Map h k v -> m (Map h k v)

setLeft    left  = onTopNode (mlLeft    .~ left)
setRight   right = onTopNode (mlRight   .~ right)
setNextKey k     = onTopNode (mlNextKey .~ k)
setPrevKey k     = onTopNode (mlPrevKey .~ k)
setValue   v     = onTopNode (mlValue   .~ v)

-- | Recalculate 'rootHash' of the node.
rehash :: forall h k v m. Stores h k v m => Map h k v -> m (Map h k v)
rehash tree = do
    layer <- open tree

    let isolated = rootHash <$> layer
    let cleaned  = isolated & mlHash .~ (defHash @h @k @v)
    let newLayer = layer    & mlHash .~ hashOf cleaned
    let newTree  = close newLayer

    saveOne newTree
    return newTree

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

-- | Create empty tree.
empty :: forall h k v . Hash h k v => Map h k v
empty = close $ MLEmpty (emptyHash @h @k @v)

emptyHash :: forall h k v . Hash h k v => h
emptyHash = hashOf (MLEmpty (defHash @h @k @v) :: MapLayer h k v h)

-- | Construct a branch from 2 subtrees.
branch :: forall h k v m. Stores h k v m => Tilt -> Map h k v -> Map h k v -> m (Map h k v)
branch tilt0 left right = do
    [minL, minR] <- traverse minKey [left, right]
    rehash $ close $ MLBranch (defHash @h @k @v) (min minL minR) minR tilt0 left right

-- | Create a leaf.
leaf :: forall h k v m. Stores h k v m => k -> v -> WithBounds k -> WithBounds k -> m (Map h k v)
leaf k v p n = do
    rehash $ close $ MLLeaf (defHash @h @k @v) (Plain k) v n p

lessThanCenterKey :: Stores h k v m => k -> Map h k v -> m Bool
lessThanCenterKey key0 = openAnd $ \layer ->
    Plain key0 < (layer^?mlCenterKey `orElse` minBound)

toList :: Stores h k v m => Map h k v -> m [(k, v)]
toList = go
  where
    go tree = do
      open tree >>= \case
        MLLeaf   {_mlKey , _mlValue} -> return [(unsafeFromWithBounds _mlKey, _mlValue)]
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
                L2 -> False
                L1 -> leftH - rightH == 1
                M  -> leftH - rightH == 0
                R1 -> leftH - rightH == -1
                R2 -> False

            leftGood  <- go _mlLeft
            rightGood <- go _mlRight

            return $ localBalance && leftGood && rightGood

          _ ->
            return True
