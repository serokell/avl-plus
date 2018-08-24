{-|
  This module houses internal representation of AVL+ tree.
  Import at your own risk.
-}

module Data.Tree.AVL.Internal where

import Control.Exception   (Exception)
import Control.Lens        (makeLenses, to, (&), (.~), (^.), (^?), (%~))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Free  (Free (Free, Pure))

import Data.Maybe          (fromJust, isNothing)
import Data.Set            (Set)
import qualified Data.Set as Set (fromList)
import Data.Tree as Tree
import Data.Typeable       (Typeable)

import GHC.Generics        (Generic)

import Data.Eq.Deriving    (deriveEq1)
import Text.Show.Deriving  (deriveShow1)

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

-- | Free 'Bounded'.
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

-- | Calls 'error' on 'Top' or 'Bottom'.
unsafeFromWithBounds :: WithBounds b -> b
unsafeFromWithBounds (Plain b) = b
unsafeFromWithBounds  _        = error "unsafeFromWithBounds: 'Top' or 'Bottom' values cannot be converted"

type Revision = Integer

-- | Representation of AVL+ tree with data in leaves.

-- | One layer of AVL Tree structure.
--   It is build that way to guarantee that 'hashOf' implementation can only touch
--   current level and be syncronised with any changes in tree.
data MapLayer h k v self
  = MLBranch
    { _mlRevision  :: Revision      -- ^ So we can check if node changed and not rehash it
    , _mlHash      :: Maybe h       -- ^ Hash of the subtree.
    , _mlMinKey    :: WithBounds k  -- ^ Minimal key used in tree.
    , _mlCenterKey :: WithBounds k  -- ^ Minimal key of the right subtree.
    , _mlTilt      :: Tilt          -- ^ Amount of disbalance.
    , _mlLeft      :: self          -- ^ Right subtree or subtree hash.
    , _mlRight     :: self          -- ^ Left subtree or subtree hash.
    }
  | MLLeaf
    { _mlRevision :: Revision
    , _mlHash     :: Maybe h
    , _mlKey      :: WithBounds k    -- ^ Key of the leaf node.
    , _mlValue    :: v               -- ^ Value of the leaf node.
    , _mlNextKey  :: WithBounds k    -- ^ Next key, for [non]existence check.
    , _mlPrevKey  :: WithBounds k    -- ^ Prev key.
    }
  | MLEmpty
    { _mlRevision :: Revision
    , _mlHash     :: Maybe h
    }
    deriving (Eq, Functor, Foldable, Traversable, Generic)

deriveEq1 ''MapLayer
deriveShow1 ''MapLayer

-- | AVL tree as whole.
type Map h k v = Free (MapLayer h k v) h

type Isolated h k v = MapLayer h k v h

newtype FreshlyRehashed h k v = FreshlyRehashed
    { getFreshlyRehashed :: Map h k v }

#if !MIN_VERSION_free(5,0,2)
deriving instance Generic (Free t a)
#endif

-------------------------------------------------------------------------------
-- * Lenses
-------------------------------------------------------------------------------

makeLenses ''MapLayer

-------------------------------------------------------------------------------
-- * Typeclasses
-------------------------------------------------------------------------------

-- | Class, representing an ability for type to be [de]serialised.
-- class Serialisable a where
--     serialise   :: a -> ByteString
--     deserialise :: ByteString -> Either String a

-- | DB monad, capable of retrieving 'isolate'd nodes.
class KVRetrieve hash node m where
    retrieve :: hash -> m node

-- | DB monad, capable of storing 'isolate'd nodes _an mass_.
class KVStore hash node m where
    massStore :: [(hash, node)] -> m ()

-- | Interface for calculating hash of the 'Map' node.
class Hash h k v where
    hashOf :: MapLayer h k v h -> h
    -- ^ Take hash of the 'MapLayer'

-- | Exception to be thrown when node with given hashkey is missing.
data NotFound k = NotFound k
    deriving (Show, Typeable)

-- | Excepton to be thrown if deserialisation is failed.
-- data DeserialisationError = DeserialisationError String
--     deriving (Show, Typeable)

-- instance Exception DeserialisationError

instance (Show k, Typeable k) => Exception (NotFound k)

-- | Umbrella class to grab all the required capabilities for tree to operate (and be debugged!).

type Base h k v m =
    ( Ord h, Show h, Typeable h
    , Ord k, Show k, Typeable k
    , Show k, Show v, Show h
    , Hash h k v
    , MonadCatch m
    )

type Stores h k v m =
    ( Base h k v m
    , KVStore h (Isolated h k v) m
    )

type Retrieves h k v m =
    ( Base h k v m
    , KVRetrieve h (Isolated h k v) m
    )

-------------------------------------------------------------------------------
-- * Methods
-------------------------------------------------------------------------------

-- | Debug preview of the tree.
showMap :: (Show h, Show k, Show v) => Map h k v -> String
showMap = drawTree . asTree
 where
   asTree = \case
     Free (MLBranch rev h _mk _ck t  l r) -> Tree.Node ("-< "    ++ show (rev) ++ ", " ++ show (h) ++ ", "  ++ show (t)) [asTree r, asTree l]
     Free (MLLeaf   rev h  k  _v _n _p)   -> Tree.Node ("<3- "   ++ show (rev) ++ ", "  ++ show (h) ++ ", "  ++ show (k)) []
     Free (MLEmpty  rev h)                -> Tree.Node ("Empty " ++ show (rev) ++ ", "  ++ show (h))             []
     Pure  h                            -> Tree.Node ("Ref "   ++ show h)               []

instance (Show h, Show k, Show v, Show self) => Show (MapLayer h k v self) where
    show = \case
      MLBranch _ h _mk _ck  t  l r -> "Branch" ++ show (h, t, l, r)
      MLLeaf   _ h  k  _v  _n _p   -> "Leaf"   ++ show (h, k)
      MLEmpty  _ h                 -> "Empty"  ++ show (h)

-- | Calculate hash outside of 'rehash'.
hashOf' :: forall h k v a. Hash h k v => MapLayer a k v h -> h
hashOf' ml = hashOf (ml & mlHash .~ Nothing)

-- | Get hash of the root node for the tree.
rootHash :: Map h k v -> Maybe h
rootHash = \case
  Pure h     -> Just h
  Free layer -> layer^.mlHash

unsafeRootHash :: Map h k v -> h
unsafeRootHash = fromJust . rootHash

walkDFS
  :: forall h k v m b res
  .  Retrieves h k v m
  =>  ( b
      , MapLayer h k v h -> b -> b
      , b -> res
      )
  -> h
  -> m res
walkDFS (start, add, finish) root = finish <$> go start (ref root)
  where
    -- We're doing it in DFS matter to save space.
    go :: b -> Map h k v -> m b
    go acc mapping = do
        open mapping >>= \point -> do
            let point' = unsafeRootHash <$> point
            case point of
              MLBranch { _mlLeft = l, _mlRight = r } -> do
                acc' <- go (add point' acc) l
                go acc' r

              MLLeaf {} -> do
                return $ add point' acc

              MLEmpty {} -> do
                return acc

-- | Fold the tree in the order of keys acsending.
fold :: Retrieves h k v m => (b, (k, v) -> b -> b, b -> res) -> h -> m res
fold (start, add, finish) = walkDFS (start, collectKVAnd add, finish)
  where
    -- We're doing it in DSF manner to save space.
    collectKVAnd :: ((k, v) -> b -> b) -> MapLayer h k v h -> b -> b
    collectKVAnd act = \case
        MLLeaf { _mlKey = k, _mlValue = v } -> act (unsafeFromWithBounds k, v)
        _other                              -> id

-- | Get set of all node hashes from a tree.
allRevisions :: forall k h v m . Retrieves h k v m => h -> m (Set Revision)
allRevisions = walkDFS @h @k @v ([], addRevision, Set.fromList)
  where
    addRevision layer = (_mlRevision layer :)

-- | Replace direct children with references on them.
-- isolate :: Retrieves h k v m => Map h k v -> m (Maybe (Map h k v))
-- isolate = openAndM $ \layer -> do
--     layer <- traverse (Pure . rootHash) layer
--     return _

-- | Replace direct children with references on them.
unsafeIsolated :: Hash h k v => MapLayer h k v (Map h k v) -> MapLayer h k v h
unsafeIsolated = fmap unsafeRootHash

-- | Unwrap the tree, possibly materializing its top node from the database.
open :: Retrieves h k v m => Map h k v -> m (MapLayer h k v (Map h k v))
open = \case
  Pure key -> do
    actual <- retrieve key
    return (Pure <$> actual)
  Free layer -> do
    return layer

-- | Unwrap the tree and apply a function.
openAnd :: Retrieves h k v m => (MapLayer h k v (Map h k v) -> a) -> Map h k v -> m a
openAnd f tree = f <$> open tree

-- | Unwrap the tree and apply a monadic action.
openAndM :: Retrieves h k v m => (MapLayer h k v (Map h k v) -> m a) -> Map h k v -> m a
openAndM f tree = f =<< open tree

-- | Wrap node layer into the tree.
close :: MapLayer h k v (Map h k v) -> Map h k v
close = Free

-- | Turn hash into unmaterialized tree.
ref :: h -> Map h k v
ref = Pure

-- | The analog to `fmap` :: (layer -> layer) -> tree -> m tree.
-- | The tree is rehashed after mapping.
onTopNode :: Retrieves h k v m => (MapLayer h k v (Map h k v) -> MapLayer h k v (Map h k v)) -> Map h k v -> m (Map h k v)
onTopNode f tree = do
    layer <- open tree
    return $ close (f layer)

assignHashes :: Hash h k v => Map h k v -> Map h k v
assignHashes = getFreshlyRehashed . fullRehash

-- | Recursively store all the materialized nodes in the database.
save :: forall h k v m . Stores h k v m => Map h k v -> m h
save tree = do
    let assigned   = assignHashes tree
    let collection = collect assigned
    massStore collection
    return (unsafeRootHash assigned)
  where
    collect :: Map h k v -> [(h, Isolated h k v)]
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
minKey = openAnd $ \layer ->
    layer^?mlMinKey `orElse`
    layer^?mlKey `orElse`
    minBound

centerKey :: Retrieves h k v m => Map h k v -> m (WithBounds k)
centerKey = openAnd $ \layer ->
    layer^?mlCenterKey `orElse`
    layer^?mlKey `orElse`
    minBound

-- | Returns balance parameter of the tree.
tilt :: Retrieves h k v m => Map h k v -> m Tilt
tilt = openAnd $ \layer ->
    layer^?mlTilt `orElse` M

setLeft     :: Retrieves h k v m => Map h k v    -> Map h k v -> m (Map h k v)
setRight    :: Retrieves h k v m => Map h k v    -> Map h k v -> m (Map h k v)
setNextKey  :: Retrieves h k v m => WithBounds k -> Map h k v -> m (Map h k v)
setPrevKey  :: Retrieves h k v m => WithBounds k -> Map h k v -> m (Map h k v)
setValue    :: Retrieves h k v m => v            -> Map h k v -> m (Map h k v)
setRevision :: Retrieves h k v m => Revision     -> Map h k v -> m (Map h k v)

setLeft     left  = onTopNode (mlLeft     .~ left)
setRight    right = onTopNode (mlRight    .~ right)
setNextKey  k     = onTopNode (mlNextKey  .~ k)
setPrevKey  k     = onTopNode (mlPrevKey  .~ k)
setValue    v     = onTopNode (mlValue    .~ v)
setRevision rev   = onTopNode $ \case
  it@ MLEmpty {} -> it
  other -> other & mlRevision .~ rev

-- | Recalculate 'rootHash' of the node.
fullRehash :: Hash h k v => Map h k v -> FreshlyRehashed h k v
fullRehash = FreshlyRehashed . go
  where
    go = \case
      Free layer -> do
        if isNothing (layer^.mlHash)
        then
            rehash $ Free $ layer
              & mlLeft  %~ go
              & mlRight %~ go
        else
            Free layer

      other ->
        other

    rehash :: forall h k v . Hash h k v => Map h k v -> Map h k v
    rehash = \case
      Free layer -> do
        let node     = unsafeIsolated layer
        let cleaned  = node  & mlHash .~ Nothing
        let newLayer = layer & mlHash .~ Just (hashOf cleaned)
        Free newLayer

      other ->
        other


another :: Side -> Side
another L = R
another R = L

infixr 1 `orElse`
orElse :: Maybe a -> a -> a
Just x `orElse` _ = x
_      `orElse` x = x

-- | For clarity of rebalance procedure.
pattern Node :: Revision -> Tilt -> a -> a -> MapLayer h k v a
pattern Node rev d l r <- MLBranch rev _ _ _ d l r

-- | Create empty tree.
empty :: forall h k v . Hash h k v => Map h k v
empty = close $ MLEmpty 0 Nothing

-- | Construct a branch from 2 subtrees.
branch :: forall h k v m. Retrieves h k v m => Revision -> Tilt -> Map h k v -> Map h k v -> m (Map h k v)
branch rev tilt0 left right = do
    [minL, minR] <- traverse minKey [left, right]
    return $ close $ MLBranch rev Nothing (min minL minR) minR tilt0 left right

-- | Create a leaf.
leaf :: forall h k v m. Retrieves h k v m => Revision -> k -> v -> WithBounds k -> WithBounds k -> m (Map h k v)
leaf rev k v p n = do
    return $ close $ MLLeaf rev Nothing (Plain k) v n p

lessThanCenterKey :: Retrieves h k v m => k -> Map h k v -> m Bool
lessThanCenterKey key0 = openAnd $ \layer ->
    Plain key0 < (layer^?mlCenterKey `orElse` minBound)

toList :: Retrieves h k v m => Map h k v -> m [(k, v)]
toList = go
  where
    go tree = do
      open tree >>= \case
        MLLeaf   {_mlKey, _mlValue = value} ->
            return [(unsafeFromWithBounds _mlKey, value)]

        MLBranch {_mlLeft, _mlRight} ->
            (++) <$> go _mlLeft <*> go _mlRight

        _ ->
            return []

size :: Retrieves h k v m => Map h k v -> m Integer
size = go
  where
    go tree = do
        open tree >>= \case
          MLEmpty  {}                  -> return 0
          MLBranch {_mlLeft, _mlRight} -> (+) <$> go _mlLeft <*> go _mlRight
          _                            -> return 1

-- | For testing purposes. Finds lengths of all paths to the leaves.
pathLengths :: Retrieves h k v m => Map h k v -> m [Int]
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

height :: Retrieves h k v m => Map h k v -> m Integer
height = go
  where
    go tree = do
        open tree >>= \case
          MLEmpty  {}                  -> return 0
          MLBranch {_mlLeft, _mlRight} -> (\x y -> 1 + max x y) <$> go _mlLeft <*> go _mlRight
          _                            -> return 1

isBalancedToTheLeaves :: forall h k v m . Retrieves h k v m => Map h k v -> m Bool
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
