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
    , MapLayer
    , MapLayerTemplate (..)
    , Isolated

      -- * High-level operations
    , rootHash
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
    , setValue
    , setLeft
    , setRight

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

      -- * Derivation helpers
    , ProvidesHash (..)
    , Combines (..)
    , beforeSerialise
    , afterDeserialise
    )
  where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Free (Free (Free, Pure))
import Lens.Micro.Platform (makeLenses, to, (&), (.~), (^.), (^?))

import Data.Maybe (fromMaybe)
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
data MapLayerTemplate t h k v self
  = MLBranch
    { _mlHash      :: ~h   -- ^ Hash of the subtree.
    , _mlMinKey    :: k    -- ^ Minimal key used in tree.
    , _mlCenterKey :: k    -- ^ Minimal key of the right subtree.
    , _mlTilt      :: t    -- ^ Amount of disbalance.
    , _mlLeft      :: self -- ^ Right subtree or subtree hash.
    , _mlRight     :: self -- ^ Left subtree or subtree hash.
    }
  | MLLeaf
    { _mlHash  :: ~h
    , _mlKey   :: k      -- ^ Key of the leaf node.
    , _mlValue :: v      -- ^ Value of the leaf node.
    }
  | MLEmpty
    { _mlHash     :: ~h
    }
    deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

type MapLayer = MapLayerTemplate Tilt

deriveEq1 ''MapLayerTemplate
deriveOrd1 ''MapLayerTemplate
deriveShow1 ''MapLayerTemplate

-- | AVL tree as whole.
type Map h k v = Free (MapLayer h k v) h

type MapTemplate t h k v = Free (MapLayerTemplate t h k v) h

-- | AVL node that was isolate (hashes were put where in places of subtrees).
type Isolated h k v = MapLayer h k v h

#if !MIN_VERSION_free(5,0,2)
deriving instance Generic (Free t a)
#endif

-------------------------------------------------------------------------------
-- * Lenses
-------------------------------------------------------------------------------

-- | Lenses.
makeLenses ''MapLayerTemplate

-------------------------------------------------------------------------------
-- * Typeclasses
-------------------------------------------------------------------------------

-- | Supposed use: `instance SomeHash a => ProvidesHash a SomeHashResult where getHash = hash`
class ProvidesHash a h | a -> h where
    getHash :: a -> h

class Combines h where
    combine :: [h] -> h

-- | Interface for calculating hash of the 'Map' node.
class Hash h k v where
    hashOf :: MapLayer h k v h -> h
    -- ^ Take hash of the 'MapLayer'

-- instance
--     ( ProvidesHash k h
--     , ProvidesHash v h
--     , ProvidesHash () h
--     , ProvidesHash Int h
--     , Combines h
--     )
--   =>
--     Hash h k v
--   where
--     hashOf = \case
--         MLBranch _ m c t l r ->
--             combine [getHash m, getHash c, getHash (fromEnum t), l, r]

--         MLLeaf _ k v ->
--             combine [getHash k, getHash v]

--         MLEmpty _ -> getHash ()

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

mapTilt :: (t1 -> t2) -> MapTemplate t1 h k v -> MapTemplate t2 h k v
mapTilt f = go
  where
    go = \case
        Free (split@ MLBranch
            { _mlTilt  = t
            , _mlLeft  = left
            , _mlRight = right
            })
          -> Free split
            { _mlTilt  = f t
            , _mlLeft  = go left
            , _mlRight = go right
            }

        Free (MLLeaf  h k v) -> Free (MLLeaf  h k v)
        Free (MLEmpty h)     -> Free (MLEmpty h)
        Pure  h              -> Pure  h

beforeSerialise :: Map h k v -> MapTemplate Int h k v
beforeSerialise = mapTilt fromEnum

afterDeserialise :: MapTemplate Int h k v -> Map h k v
afterDeserialise = mapTilt toEnum

-- | Debug preview of the tree.
showMap :: (Show h, Show k, Show v) => Map h k v -> String
showMap = Tree.drawTree . asTree
  where
    asTree = \case
      Free (MLBranch h m c t l r) -> Tree.Node ("Branch " ++ show (h, m, c, t)) [asTree r, asTree l]
      Free (MLLeaf   h k v)       -> Tree.Node ("Leaf   " ++ show (h, k, v))    []
      Free (MLEmpty  h)           -> Tree.Node ("Empty  " ++ show (h))          []
      Pure  h                     -> Tree.Node ("Ref    " ++ show h)            []

-- | Calculate hash outside of 'rehash'.
hashOf' :: forall h k v. Hash h k v => MapLayer h k v h -> h
hashOf' = hashOf . protect
  where
    protect ml = ml
        & mlHash .~ error "Data.Tree.AVL.Internal.hashOf': old hash is used to generate new one"

-- | Get hash of the root node for the tree.
rootHash :: Map h k v -> h
rootHash = \case
    Pure h     -> h
    Free layer -> layer^.mlHash

-- | Replace direct children with references on them.
isolate :: Hash h k v => MapLayer h k v (Map h k v) -> MapLayer h k v h
isolate = fmap rootHash

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
close :: Hash h k v => MapLayer h k v (Map h k v) -> Map h k v
close = Free . rehashLayer
  where
    rehashLayer layer = layer & mlHash .~ hashOf' (isolate layer)

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
    return $ close $ f layer

-- | Recursively store all the materialized nodes in the database.
save :: forall h k v m . Stores h k v m => Map h k v -> m (Map h k v)
save tree = do
    massStore (collect tree)
    return (ref (rootHash tree))

-- | Turns a 'Map' into relation of (hash, isolated-node),
--   to use in 'save'.
collect :: forall h k v . Hash h k v => Map h k v -> [(h, Isolated h k v)]
collect it = case it of
    Pure _     -> []
    Free layer -> do
        let hash  = rootHash it
        let node  = isolate (layer :: MapLayer h k v (Map h k v))
        let left  = layer^?mlLeft .to collect `orElse` []
        let right = layer^?mlRight.to collect `orElse` []

        ((hash, node) : (left ++ right))

-- | Returns minimal key contained in a tree (or a 'minbound' if empty).
minKey :: Retrieves h k v m => Map h k v -> m (WithBounds k)
minKey = loadAnd $ \layer ->
    layer^?mlMinKey.to Plain `orElse`
    layer^?mlKey.to Plain `orElse`
    Bottom

-- | Returns minimal right key contained in a tree (or a 'minbound' if empty).
centerKey :: Retrieves h k v m => Map h k v -> m (WithBounds k)
centerKey = loadAnd $ \layer ->
    layer^?mlCenterKey.to Plain `orElse`
    layer^?mlKey.to Plain `orElse`
    Top

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
setValue :: Retrieves h k v m => v -> Map h k v -> m (Map h k v)
setValue v = onTopNode (mlValue .~ v)

-- | Recalculate 'rootHash' of the node.
--
--   Does so recursively. Will not go below node with already calculated hash
--   or unloaded one.
-- | Switch side.
another :: Side -> Side
another L = R
another R = L

-- | Just better name for 'flip' 'fromMaybe'.
infixr 1 `orElse`
orElse :: Maybe a -> a -> a
orElse = flip fromMaybe

-- | For clarity of rebalance procedure.
pattern Node :: h -> Tilt -> a -> a -> MapLayer h k v a
pattern Node h d l r <- MLBranch h _ _ d l r

-- | Root hash of the empty tree.
emptyHash :: forall h k v. Hash h k v => h
emptyHash = rootHash (empty @_ @k @v)

-- | Create empty tree. Hash is not set.
empty :: forall h k v . Hash h k v => Map h k v
empty = close $ MLEmpty { _mlHash = undefined }

-- | Construct a branch from 2 subtrees. Hash is not set.
branch :: Retrieves h k v m => Tilt -> Map h k v -> Map h k v -> m (Map h k v)
branch tilt0 left right = do
    [minL, minR] <- traverse minKey [left, right]
    return $ close $ MLBranch
        { _mlHash      = undefined
        , _mlMinKey    = unsafeFromWithBounds $ min minL minR
        , _mlCenterKey = unsafeFromWithBounds   minR
        , _mlTilt      = tilt0
        , _mlLeft      = left
        , _mlRight     = right
        }

-- | Create a leaf. Hash is not set.
leaf :: Retrieves h k v m => k -> v -> m (Map h k v)
leaf k v = return $ close $ MLLeaf { _mlHash = undefined, _mlKey = k, _mlValue = v }

-- | Disband tree into assoc list.
toList :: Retrieves h k v m => Map h k v -> m [(k, v)]
toList = go
  where
    go tree = do
      load tree >>= \case
        MLLeaf   {_mlKey,  _mlValue} -> return [(_mlKey, _mlValue)]
        MLBranch {_mlLeft, _mlRight} -> (++) <$> go _mlLeft <*> go _mlRight
        MLEmpty  {}                  -> return []

-- | Calculate count of nodes.
size :: Retrieves h k v m => Map h k v -> m Integer
size = go
  where
    go tree = load tree >>= \case
        MLBranch {_mlLeft, _mlRight} -> (+) <$> go _mlLeft <*> go _mlRight
        MLLeaf   {}                  -> return 1
        MLEmpty  {}                  -> return 0

-- | For testing purposes. Finds lengths of all paths to the leaves.
pathLengths :: Retrieves h k v m => Map h k v -> m [Int]
pathLengths = go
  where
    go tree = load tree >>= \case
        MLLeaf {} ->
            return [0]

        MLBranch {_mlLeft, _mlRight} -> do
            lefts  <- go _mlLeft
            rights <- go _mlRight
            return $ map (+ 1) $ lefts ++ rights

        MLEmpty {} ->
            return []

height :: Retrieves h k v m => Map h k v -> m Integer
height = go
  where
    go tree = load tree >>= \case
        MLBranch {_mlLeft, _mlRight} -> (\x y -> 1 + max x y) <$> go _mlLeft <*> go _mlRight
        MLLeaf   {}                  -> return 1
        MLEmpty  {}                  -> return 0

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
