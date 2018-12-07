{-|
  This module houses internal representation of AVL+ tree. It's not
  designed to be imported from the outside.
-}

module Data.Tree.AVL.Internal
    ( -- * Interfaces with database
      KVRetrieve (..)
    , KVStore (..)

      -- * Contexts for tree operations
    , Retrieves
    , Stores
    , Params
    , Base

      -- * Interface to plug in hash
    , Hash (..)
    , hashOf'

      -- * Exception to be thrown by DB operations
    , NotFound (..)

      -- * AVL map type, its layer and variants
    , Map
    , MapLayer (..)
    , Isolated

    , unsafeRootHash
    , unsafeLayerHash

      -- * High-level operations
    , rootHash
    , fullRehash
    , save
    , size
    , toList

      -- * Smart constructors
    , ref
    , branch
    , leaf
    , empty
    , emptyHash

      -- * Getters
    , centerKey
    , tilt

      -- * Setters
    , setNextKey
    , setPrevKey
    , setValue
    , setLeft
    , setRight
    , clearHash

      -- * Introspection
    , load
    , loadAnd
    , loadAndM

      -- * Low-level access
    , pattern Node
    , mlKey
    , mlLeft
    , mlRight
    , mlValue
    , mlHash
    , mlTilt

      -- * Key wrapper, free 'Bounded'
    , WithBounds (..)
    , fromWithBounds
    , unsafeFromWithBounds

      -- * Amount of disbalance
    , Tilt (..)

      -- * Navigation helper
    , Side (..)
    , another

      -- * Debug-related operations
    , isBalancedToTheLeaves
    , showMap
    , pathLengths
    , orElse
    )
  where

import Control.Exception (Exception)
import Control.Lens (makeLenses, to, view, (%~), (&), (.~), (^.), (^?))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Free (Free (Free, Pure))
import Control.Monad.IO.Class (MonadIO)

import Data.Maybe (fromJust, fromMaybe, isNothing)
import qualified Data.Tree as Tree
import Data.Typeable (Typeable)

import GHC.Generics (Generic)

import Data.Eq.Deriving (deriveEq1)
import Data.Ord.Deriving (deriveOrd1)
import Text.Show.Deriving (deriveShow1)

-------------------------------------------------------------------------------
-- * Datatypes
-------------------------------------------------------------------------------

-- | Direction of downward step in tree iteration.
data Side
    = L
    | R
    deriving (Eq, Show, Generic)

-- | Difference in branches heights.
data Tilt
    = L2  -- ^ UnbalancedLeft
          --
          -- prop> h(L) = h(R) + 2
    | L1  -- ^ BalancedLeft
          --
          -- prop> h(L) = h(R) + 1
    | M   -- ^ Balanced
          --
          -- prop> h(L) = h(R)
    | R1  -- ^ BalancedRight
          --
          -- prop> h(R) = h(L) + 1
    | R2  -- ^ UnbalancedRight
          --
          -- prop> h(R) = h(L) + 2
    deriving (Eq, Ord, Show, Enum, Generic)

-- | Makes 'Bounded' from everything by explicitly adding top/bottom.
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

-- | Retrieve @b@ from 'WithBounds' @b@.
--   Both 'Top' and 'Bottom' result in 'Nothing'.
fromWithBounds :: WithBounds b -> Maybe b
fromWithBounds (Plain b) = Just b
fromWithBounds  _        = Nothing

-- | 'fromWithBounds' that calls 'error' on 'Top' or 'Bottom'.
unsafeFromWithBounds :: WithBounds b -> b
unsafeFromWithBounds = fromMaybe err . fromWithBounds
  where
    err = error "unsafeFromWithBounds: 'Top' or 'Bottom' values cannot be converted"

-- | Representation of AVL+ tree with data in leaves.

-- | One layer of AVL Tree structure.
--   It is build that way to guarantee that 'hashOf' implementation can only touch
--   current level and be syncronised with any changes in tree.
data MapLayer h k v self
  = MLBranch
    { _mlHash      :: Maybe h       -- ^ Hash of the subtree.
    , _mlMinKey    :: WithBounds k  -- ^ Minimal key used in tree.
    , _mlCenterKey :: WithBounds k  -- ^ Minimal key of the right subtree.
    , _mlTilt      :: Tilt          -- ^ Amount of disbalance.
    , _mlLeft      :: self          -- ^ Right subtree or subtree hash.
    , _mlRight     :: self          -- ^ Left subtree or subtree hash.
    }
  | MLLeaf
    { _mlHash    :: Maybe h
    , _mlKey     :: WithBounds k    -- ^ Key of the leaf node.
    , _mlValue   :: v               -- ^ Value of the leaf node.
    , _mlNextKey :: WithBounds k    -- ^ Next key, for [non]existence check.
    , _mlPrevKey :: WithBounds k    -- ^ Previous key.
    }
  | MLEmpty
    { _mlHash     :: Maybe h
    }
    deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

deriveEq1 ''MapLayer
deriveOrd1 ''MapLayer
deriveShow1 ''MapLayer

-- | AVL tree as whole.
type Map h k v = Free (MapLayer h k v) h

-- | AVL node that was isolated (hashes were put where in places of subtrees).
type Isolated h k v = MapLayer h k v h

#if !MIN_VERSION_free(5,0,2)
deriving instance Generic (Free t a)
#endif

-------------------------------------------------------------------------------
-- * Lenses
-------------------------------------------------------------------------------

-- | Lenses.
makeLenses ''MapLayer

-------------------------------------------------------------------------------
-- * Typeclasses
-------------------------------------------------------------------------------

-- | Interface for calculating hash of the 'Map' node.
class Hash h k v where
    hashOf :: MapLayer h k v h -> h
    -- ^ Take hash of the 'MapLayer'

-- | DB monad capable of retrieving 'isolate'd nodes.
class KVRetrieve hash node m where
    retrieve :: hash -> m node

-- | DB monad capable of storing 'isolate'd nodes altogether.
class KVStore hash node m where
    massStore :: [(hash, node)] -> m ()

-- | Exception to be thrown when node with given hashkey is missing.
data NotFound k = NotFound k
    deriving (Show, Typeable)


instance (Show k, Typeable k) => Exception (NotFound k)

-- | Constraints on type parameters for AVL 'Map'.
type Params h k v =
    ( Ord h, Show h, Typeable h
    , Ord k, Show k, Typeable k
           , Show v, Typeable v
    , Hash h k v
    )

-- | Umbrella constraint to grab all the required capabilities for
-- tree to operate.
type Base h k v m =
    ( Params h k v
    , MonadCatch m
    , MonadIO m
    )

-- | Ability to write into the storage.
type Stores h k v m =
    ( Base h k v m
    , KVStore h (Isolated h k v) m
    )

-- | Ability to read from the storage.
type Retrieves h k v m =
    ( Base h k v m
    , KVRetrieve h (Isolated h k v) m
    )

-------------------------------------------------------------------------------
-- * Methods
-------------------------------------------------------------------------------

-- | Debug preview of the tree.
showMap :: (Show h, Show k, Show v) => Map h k v -> String
showMap = Tree.drawTree . asTree
  where
    asTree = \case
      Free (MLBranch h _mk _ck t  l r) -> Tree.Node ("Branch " ++ show (h, _mk, _ck, t))    [asTree r, asTree l]
      Free (MLLeaf   h  k   v _n _p)   -> Tree.Node ("Leaf   " ++ show (h, k, v, _n, _p)) []
      Free (MLEmpty  h)                -> Tree.Node ("Empty  " ++ show (h))       []
      Pure  h                              -> Tree.Node ("Ref    " ++ show h)              []

-- instance (Show h, Show k, Show v, Show self) => Show (MapLayer h k v self) where
--     show = \case
--       MLBranch _ h _mk _ck  t  l r -> "Branch" ++ show (h, t, l, r)
--       MLLeaf   _ h  k   v  _n _p   -> "Leaf"   ++ show (h, k, v)
--       MLEmpty  _ h                 -> "Empty"  ++ show (h)

-- | Calculate hash outside of 'rehash'.
hashOf' :: forall h k v a. Hash h k v => MapLayer a k v h -> h
hashOf' ml = hashOf (ml & mlHash .~ Nothing)

-- | Get hash of the root node for the tree.
rootHash :: Map h k v -> Maybe h
rootHash = \case
    Pure h     -> Just h
    Free layer -> layer^.mlHash

-- | Get the root hash in a unsafe manner.
unsafeRootHash :: Map h k v -> h
unsafeRootHash = fromMaybe (error "unsafeRootHash failed") . rootHash

-- | Get the layer hash in an unsafe manner.
unsafeLayerHash :: MapLayer h k v c -> h
unsafeLayerHash = fromMaybe (error "unsafeLayerHash failed") . view mlHash

-- | Replace direct children with references on them.
unsafeIsolated :: Hash h k v => MapLayer h k v (Map h k v) -> MapLayer h k v h
unsafeIsolated = fmap unsafeRootHash

-- | Unwrap the tree, possibly materializing its top node from the database.
load :: Retrieves h k v m => Map h k v -> m (MapLayer h k v (Map h k v))
load = \case
    Pure key -> do
        actual <- retrieve key
        return (Pure <$> actual)
    Free layer ->
        return layer

-- | Unwrap the tree and apply a function.
loadAnd :: Retrieves h k v m => (MapLayer h k v (Map h k v) -> a) -> Map h k v -> m a
loadAnd f tree = f <$> load tree

-- | Unwrap the tree and apply a monadic action.
loadAndM :: Retrieves h k v m => (MapLayer h k v (Map h k v) -> m a) -> Map h k v -> m a
loadAndM f tree = f =<< load tree

-- | Wrap node layer into the tree.
close :: MapLayer h k v (Map h k v) -> Map h k v
close = Free

-- | Turn hash into unmaterialized tree.
ref :: h -> Map h k v
ref = Pure

-- | The analog to `fmap` :: (layer -> layer) -> tree -> m tree.
-- | The tree is rehashed after mapping.
onTopNode ::
       Retrieves h k v m
    => (MapLayer h k v (Map h k v) -> MapLayer h k v (Map h k v))
    -> Map h k v
    -> m (Map h k v)
onTopNode f tree = do
    layer <- load tree
    return $ close (f layer)

-- | Recursively store all the materialized nodes in the database.
save :: forall h k v m . Stores h k v m => Map h k v -> m (Map h k v)
save tree = do
    let rehashed   = fullRehash tree
    let collection = collect rehashed
    massStore collection
    return (ref (unsafeRootHash rehashed))

-- | Turns a 'Map' into relation of (hash, isolated-node),
--   to use in 'save'.
collect :: Hash h k v => Map h k v -> [(h, Isolated h k v)]
collect it = case it of
    Pure _ -> []
    Free layer -> do
        let hash  = unsafeRootHash it
        let node  = unsafeIsolated layer
        let left  = layer^?mlLeft .to collect `orElse` []
        let right = layer^?mlRight.to collect `orElse` []

        ((hash, node) : (left ++ right))

-- | Returns minimal key contained in a tree (or a 'minbound' if empty).
minKey :: Retrieves h k v m => Map h k v -> m (WithBounds k)
minKey = loadAnd $ \layer ->
    layer^?mlMinKey `orElse`
    layer^?mlKey `orElse`
    minBound

-- | Returns minimal right key contained in a tree (or a 'minbound' if empty).
centerKey :: Retrieves h k v m => Map h k v -> m (WithBounds k)
centerKey = loadAnd $ \layer ->
    layer^?mlCenterKey `orElse`
    layer^?mlKey `orElse`
    minBound

-- | Returns balance parameter of the tree.
tilt :: Retrieves h k v m => Map h k v -> m Tilt
tilt = loadAnd $ \layer ->
    layer^?mlTilt `orElse` M

-- | Sets left subtree in a branch.
setLeft :: Retrieves h k v m => Map h k v -> Map h k v -> m (Map h k v)
setLeft left = onTopNode (mlLeft.~ left)

-- | Sets right subtree in a branch.
setRight :: Retrieves h k v m => Map h k v-> Map h k v -> m (Map h k v)
setRight right = onTopNode (mlRight .~ right)

-- | Sets next key in a leaf.
setNextKey :: Retrieves h k v m => WithBounds k -> Map h k v -> m (Map h k v)
setNextKey k = onTopNode (mlNextKey .~ k)

-- | Sets prev key in a leaf.
setPrevKey :: Retrieves h k v m => WithBounds k -> Map h k v -> m (Map h k v)
setPrevKey k = onTopNode (mlPrevKey .~ k)

-- | Sets next key in a leaf.
setValue :: Retrieves h k v m => v -> Map h k v -> m (Map h k v)
setValue v = onTopNode (mlValue .~ v)

-- | Wipes node hash out.
clearHash :: Retrieves h k v m => Map h k v -> m (Map h k v)
clearHash = onTopNode $ mlHash .~ Nothing

-- | Recalculate 'rootHash' of the node.
--
--   Does so recursively. Will not go below node with already calculated hash
--   or unloaded one.
fullRehash :: Hash h k v => Map h k v -> Map h k v
fullRehash = go
  where
    go = \case
        Free layer -> do
            if isNothing (layer^.mlHash)
            then rehash $ Free $ layer & mlLeft  %~ go
                                       & mlRight %~ go
            else Free layer
        other -> other

    rehash :: forall h k v . Hash h k v => Map h k v -> Map h k v
    rehash = \case
        Free layer -> do
            let node     = unsafeIsolated layer
            let cleaned  = node  & mlHash .~ Nothing
            let newLayer = layer & mlHash .~ Just (hashOf cleaned)
            Free newLayer
        other -> other

-- | Switch side.
another :: Side -> Side
another L = R
another R = L

-- | Just better name for 'flip' 'fromMaybe'.
infixr 1 `orElse`
orElse :: Maybe a -> a -> a
orElse = flip fromMaybe

-- | For clarity of rebalance procedure.
pattern Node :: Maybe h -> Tilt -> a -> a -> MapLayer h k v a
pattern Node h d l r <- MLBranch h _ _ d l r

-- | Root hash of the empty tree.
emptyHash :: forall h k v. Hash h k v => h
emptyHash = fromJust $ rootHash $ fullRehash (empty @_ @k @v)

-- | Create empty tree. Hash is not set.
empty :: forall h k v . Hash h k v => Map h k v
empty = close $ MLEmpty Nothing

-- | Construct a branch from 2 subtrees. Hash is not set.
branch :: Retrieves h k v m => Tilt -> Map h k v -> Map h k v -> m (Map h k v)
branch tilt0 left right = do
    [minL, minR] <- traverse minKey [left, right]
    return $ close $ MLBranch Nothing (min minL minR) minR tilt0 left right

-- | Create a leaf. Hash is not set.
leaf :: Retrieves h k v m => k -> v -> WithBounds k -> WithBounds k -> m (Map h k v)
leaf k v p n = return $ close $ MLLeaf Nothing (Plain k) v n p

-- | Disband tree into assoc list.
toList :: Retrieves h k v m => Map h k v -> m [(k, v)]
toList = go
  where
    go tree = do
      load tree >>= \case
        MLLeaf   {_mlKey, _mlValue = value} ->
            return [(unsafeFromWithBounds _mlKey, value)]

        MLBranch {_mlLeft, _mlRight} ->
            (++) <$> go _mlLeft <*> go _mlRight

        _ -> return []

-- | Calculate count of nodes.
size :: Retrieves h k v m => Map h k v -> m Integer
size = go
  where
    go tree = load tree >>= \case
        MLEmpty  {}                  -> return 0
        MLBranch {_mlLeft, _mlRight} -> (+) <$> go _mlLeft <*> go _mlRight
        _                            -> return 1

-- | For testing purposes. Finds lengths of all paths to the leaves.
pathLengths :: Retrieves h k v m => Map h k v -> m [Int]
pathLengths = go
  where
    go tree = load tree >>= \case
        MLLeaf   {}                  -> return [0]
        MLBranch {_mlLeft, _mlRight} -> do
            lefts  <- go _mlLeft
            rights <- go _mlRight
            return $ map (+ 1) $ lefts ++ rights
        _                            -> return []

height :: Retrieves h k v m => Map h k v -> m Integer
height = go
  where
    go tree = load tree >>= \case
        MLEmpty  {}                  -> return 0
        MLBranch {_mlLeft, _mlRight} -> (\x y -> 1 + max x y) <$> go _mlLeft <*> go _mlRight
        _                            -> return 1

-- | Checks that for each branch difference between its siblings is <= 1.
--
-- There are two patological "diagonal" cases which are not checked here and will not
-- be fixed by rebalancing: when each left sibling is +1 to the right one and vice versa.
isBalancedToTheLeaves :: forall h k v m . Retrieves h k v m => Map h k v -> m Bool
isBalancedToTheLeaves = go
  where
    go :: Map h k v -> m Bool
    go tree =  load tree >>= \case
        MLBranch {_mlLeft, _mlRight, _mlTilt} -> do
            leftH  <- height _mlLeft
            rightH <- height _mlRight
            let localBalance = case _mlTilt of
                    L2 -> False
                    L1 -> leftH - rightH == 1
                    M  -> leftH - rightH == 0
                    R1 -> leftH - rightH == -1
                    R2 -> False

            leftGood  <- go _mlLeft
            rightGood <- go _mlRight

            return $ localBalance && leftGood && rightGood

        _ -> return True
