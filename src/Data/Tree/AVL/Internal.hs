{-|
  This module houses internal representation of AVL+ tree. It's not
  designed to be imported from the outside.
-}

module Data.Tree.AVL.Internal
    ( -- * Interfaces with database
      Retrieves (..)

      -- * Interface to plug in hash
    , ProvidesHash (..)
    , Hash
    , Debug
    , hashOf

      -- * Exception to be thrown by DB operations
    , NotFound (..)
    , notFound

      -- * AVL map type, its layer and variants
    , Map
    , MapLayer (..)
    , Rep
    , toRep
    , fromRep

      -- * High-level operations
    , rootHash
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
    , Tilt
    , pattern L2
    , pattern L1
    , pattern M
    , pattern R1
    , pattern R2
    , tiltLeft
    , tiltRight

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
import Control.Monad.Catch (MonadCatch, MonadThrow (throwM))
import Control.Monad.Free (Free (Free, Pure), iter)
import Control.Lens (makeLenses, to, (&), (.~), (^.), (^?))

import Data.Function (on)
import Data.Maybe (fromMaybe)
import qualified Data.Tree as Tree
import Data.Word (Word8)

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
type Tilt = Word8

pattern L2 :: Tilt
pattern L2 = 0
-- ^ UnbalancedLeft
--
-- prop> h(L) = h(R) + 2

pattern L1 :: Tilt
pattern L1 = 1
-- ^ BalancedLeft
--
-- prop> h(L) = h(R) + 1

pattern M :: Tilt
pattern M = 2
-- ^ Balanced
--
-- prop> h(L) = h(R)

pattern R1 :: Tilt
pattern R1 = 3
-- ^ BalancedRight
--
-- prop> h(R) = h(L) + 1

pattern R2 :: Tilt
pattern R2 = 4
-- ^ UnbalancedRight
--
-- prop> h(R) = h(L) + 2

tiltLeft :: Tilt -> Tilt
tiltLeft t
    | t < 1    = error "tiltLeft: from L2"
    | otherwise = t - 1

tiltRight :: Tilt -> Tilt
tiltRight t
    | t > 3     = error "tiltRight: from R2"
    | otherwise = t + 1

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
--
--   One layer of AVL Tree structure.
--   It is build that way to guarantee that 'hashOf' implementation can only touch
--   current level and be syncronised with any changes in tree.
data MapLayer h k v self
  = MLBranch
    { _mlHash      :: ~h   -- ^ Hash of the subtree.
    , _mlMinKey    :: k    -- ^ Minimal key used in tree.
    , _mlCenterKey :: k    -- ^ Minimal key of the right subtree.
    , _mlTilt      :: Tilt -- ^ Amount of disbalance.
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
    deriving (Functor, Foldable, Traversable)

type Rep h k v
  = Either (h, k, k, Tilt, h, h)
  ( Either (h, k, v)
            h )

deriveEq1 ''MapLayer
deriveOrd1 ''MapLayer
deriveShow1 ''MapLayer

-- | AVL tree as whole.
type Map h k v = Free (MapLayer h k v) h

-------------------------------------------------------------------------------
-- * Lenses
-------------------------------------------------------------------------------

-- | Lenses.
makeLenses ''MapLayer

toRep :: forall h k v. MapLayer h k v (Map h k v) -> Rep h k v
toRep (MLBranch a b c d e f) = Left          (a, b, c, d, rootHash e, rootHash f)
toRep (MLLeaf   a b c)       = Right $ Left  (a, b, c)
toRep (MLEmpty  a)           = Right $ Right  a

fromRep :: Rep h k v -> MapLayer h k v (Map h k v)
fromRep
  = either (\(a, b, c, d, e, f) -> MLBranch a b c d (Pure e) (Pure f))
  $ either (\(a, b, c)          -> MLLeaf   a b c)
           (\ a                 -> MLEmpty  a)

-------------------------------------------------------------------------------
-- * Instances
-------------------------------------------------------------------------------

instance {-# OVERLAPPING #-} Eq h => Eq (Map h k v) where
    (==) = (==) `on` rootHash

instance Eq h => Eq (MapLayer h k v h) where
    (==) = (==) `on` (^.mlHash)

-------------------------------------------------------------------------------
-- * Typeclasses
-------------------------------------------------------------------------------

-- | Intended use:
--
--  > instance SomeHash a => ProvidesHash a SomeHashResult where getHash = hash
--
--  Is used to plug in an implementation of hashing.
class ProvidesHash a h | a -> h where
    getHash :: a -> h

type Debug h k v = (Show h, Show k, Show v)

-- | Interface for calculating hash of the 'Map' node.
type Hash h k v =
    ( ProvidesHash k h
    , ProvidesHash v h
    , ProvidesHash () h
    , ProvidesHash Word8 h
    , ProvidesHash [h] h
    )

-- | Calculate hash of one layer.
hashOf :: Hash h k v => Rep h k v -> h
hashOf = \case
    Left  (_, m, c, t, l, r) -> getHash [getHash m, getHash c, getHash t, l, r]
    Right (Left  (_, k, v))  -> getHash [getHash k, getHash v]
    Right (Right (_))        -> getHash ()

-- | DB monad capable of retrieving 'isolate'd nodes.
class
    ( Ord h
    , Ord k
    , Show h
    , Show k
    , Hash h k v
    , MonadCatch m
    )
  =>
    Retrieves h k v m
      | m -> h k v
  where
    retrieve :: h -> m (Rep h k v)

-- | Exception to be thrown when node with given hashkey is missing.
newtype NotFound = NotFound String
    deriving anyclass Exception

instance Show NotFound where
  show (NotFound s) = "NotFound: " ++ s

notFound :: (MonadThrow m, Show k) => k -> m a
notFound = throwM . NotFound . show

instance {-# OVERLAPS #-} (Show h, Show k, Show v) => Show (Map h k v) where
  show = showMap

-------------------------------------------------------------------------------
-- * Methods
-------------------------------------------------------------------------------

-- | Debug preview of the tree.
showMap :: (Show h, Show k, Show v) => Map h k v -> String
showMap = Tree.drawTree . asTree
  where
    asTree = \case
      Free (MLBranch h m c t l r) -> Tree.Node ("Branch " ++ show (h, m, c, t)) [asTree r, asTree l]
      Free (MLLeaf   h k v)       -> Tree.Node ("Leaf " ++ show (h, k, v))    []
      Free (MLEmpty  h)           -> Tree.Node ("Empty " ++ show (h))          []
      Pure  h                     -> Tree.Node ("Ref  " ++ show h)            []

-- | Get hash of the root node for the tree.
rootHash :: Map h k v -> h
rootHash = \case
    Pure h     -> h
    Free layer -> layer^.mlHash

-- | Unwrap the tree, possibly materializing its top node from the database.
load :: Retrieves h k v m => Map h k v -> m (MapLayer h k v (Map h k v))
load = \case
    Pure key -> do
        actual <- retrieve key
        return (fromRep actual)
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
    rehashLayer layer = layer & mlHash .~ hashOf (toRep layer)

-- | Turn hash into unmaterialized tree.
ref :: h -> Map h k v
ref = Pure

-- | The analog to `fmap` :: (layer -> layer) -> tree -> m tree.
--
--   The tree is rehashed after mapping.
onTopNode ::
       Retrieves h k v m
    => (MapLayer h k v (Map h k v) -> MapLayer h k v (Map h k v))
    -> Map h k v
    -> m (Map h k v)
onTopNode f tree = do
    layer <- load tree
    return $ close $ f layer

-- | Returns minimal key contained in a tree (or a 'minbound' if empty).
minKey :: Retrieves h k v m => Map h k v -> m (WithBounds k)
minKey = loadAnd $ \layer ->
    layer^?mlMinKey.to Plain `orElse`
    layer^?mlKey   .to Plain `orElse`
    Bottom

-- | Returns minimal right key contained in a tree (or a 'minbound' if empty).
centerKey :: Retrieves h k v m => Map h k v -> m (WithBounds k)
centerKey = loadAnd $ \layer ->
    layer^?mlCenterKey.to Plain `orElse`
    layer^?mlKey      .to Plain `orElse`
    Top

-- | Returns balance parameter of the tree.
tilt :: Retrieves h k v m => Map h k v -> m Tilt
tilt = loadAnd $ \layer ->
    layer^?mlTilt `orElse` M

-- | Sets left subtree in a branch.
setLeft :: Retrieves h k v m => Map h k v -> Map h k v -> m (Map h k v)
setLeft left = onTopNode (mlLeft.~ left)

-- | Sets right subtree in a branch.
setRight :: Retrieves h k v m => Map h k v -> Map h k v -> m (Map h k v)
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
    ~[minL, minR] <- traverse minKey [left, right]
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
                    _  -> error "isBalancedToTheLeaves: tilt has break loose"

            leftGood  <- go _mlLeft
            rightGood <- go _mlRight

            return $ localBalance && leftGood && rightGood

        _ -> return True
