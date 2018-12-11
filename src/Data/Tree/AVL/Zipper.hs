-- | This module represents zipper layer.
--   It allows us represent all operations using
--     'up', 'descentLeft', 'descentRight' and 'change'
--   primitives.
--   The 'up' also rebalances and recalculates a node, if you 'change'd it
--   or its sibling.

module Data.Tree.AVL.Zipper
    ( -- * Iterator monad and two ways of launching it
      Zipped
    , runZipped
    , runZipped'

      -- * Mode of operation
    , Mode (..)

      -- * Navigation
    , goto
    , gotoPrevKey
    , gotoNextKey
    , up
    , descentLeft
    , descentRight

      -- * Actions over iterator
    , withLocus
    , change
    , replaceWith
    , mark

      -- * Current point iterator is standing on
    , locus

      -- * Debug
    , say
    , dump
    )
  where

import Control.Exception (Exception, SomeException)
import Lens.Micro.Platform (Lens', SimpleGetter, makeLenses, use, (%=), (.=), (<&>))

import Control.Monad (unless, when)
import Control.Monad.Catch (catch, throwM)
import Control.Monad.State.Strict (StateT, evalStateT, lift, liftIO)

import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Typeable (Typeable)

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof
import Data.Tree.AVL.Prune

import qualified Data.Set as Set (empty, fromList, insert)

-- | Zipper representation.
--
--   Notice that in no case can zipper modify the tree.
--   It will only create a new one with updates applied.
--
--   Zipper is pair of _locus_ and _context stack_.
--   'locus' is a place we're currently standing on.
--   Context stack is all the layers we have descent through to reach
--   where we are now.
--
--   Initially, the 'locus' is root node, and the context stack is empty.
--
--   After 'descentLeft' the 'locus' points to left branch of the root, and
--   the context stack will contain one layer of 'TreeZipperCtx', containing
--   data to return onto root node and update it if needed.
--
--   Zipper is also a functional iterator.
--
data TreeZipper h k v = TreeZipper
    { _tzContext  :: [TreeZipperCtx h k v]  -- ^ Layers above us
    , _tzHere     ::  Map h k v             -- ^ Current point
    , _tzKeyRange :: (Range k)              -- ^ Range of keys covered by locus
    , _tzMode     ::  Mode                  -- ^ Update or delete
    , _tzTouched  ::  Set h                 -- ^ Set of nodes we touched
    , _tzDirty    ::  Bool
    }

type Range k = (WithBounds k, WithBounds k)

-- | Tree layers.
data TreeZipperCtx h k v = TreeZipperCtx
    { _tzcSize  :: Side       -- ^ the direction we came from
    , _tzcFrom  :: Map h k v  -- ^ the node we came from (parent)
    , _tzcRange :: Range k    -- ^ the key range we were at
    , _tzcDirty :: Bool
    }
    deriving Show

--deriving instance Retrieves h k v => Show (TreeZipperCxt h k v)

-- | Modes of operation.
--
--   The reason for this type to exist is that rebalancer will confuse
--   situations where tree was enlarged by one operation and shrinked by another.
--
--   That is because tree only stores difference between heights of siblings
--   - 'Tilt' - so we can omit their materialisation in rebalance.
data Mode
    = UpdateMode    -- ^ The tree should only grow in that mode
    | DeleteMode    -- ^ The tree should only shrink in that mode
    | ReadonlyMode  -- ^ The tree cannot be updated at all
    deriving (Show, Eq)

makeLenses ''TreeZipper

-- | Current subtree.
locus    :: Lens'  (TreeZipper h k v) (Map h k v)
context  :: Lens'  (TreeZipper h k v) [TreeZipperCtx h k v]
keyRange :: Lens'  (TreeZipper h k v) (Range k)
trail    :: Lens'  (TreeZipper h k v) (Set h)
mode     :: SimpleGetter (TreeZipper h k v)  Mode

mode     = tzMode
keyRange = tzKeyRange
locus    = tzHere
context  = tzContext
trail    = tzTouched

-- | Monad, carrying zipper as state.
type Zipped h k v m = StateT (TreeZipper h k v) m

instance
    ( Monad m
    , KVRetrieve h (Isolated h k v) m
    )
  =>
    KVRetrieve h (Isolated h k v) (Zipped h k v m)
  where
    retrieve = lift . retrieve

-- | Run zipper operation, collect prefabricated proofs.
runZipped :: Retrieves h k v m => Zipped h k v m a -> Mode -> Map h k v -> m (a, Map h k v, Set h)
runZipped action mode0 tree = do
    action' `evalStateT` enter mode0 tree
  where
    action' = do
      res    <- action
      tree'  <- exit
      trails <- use trail
      return (res, tree', trails)

-- | Run zipper operation, build proof.
runZipped' :: Retrieves h k v m => Zipped h k v m a -> Mode -> Map h k v -> m (a, Map h k v, Proof h k v)
runZipped' action mode0 tree = do
    (a, tree1, trails) <- runZipped action mode0 tree
    proof              <- prune trails tree
    return (a, tree1, proof)

say :: Retrieves h k v m => String -> Zipped h k v m ()
say = liftIO . putStrLn

dump :: Retrieves h k v m => Zipped h k v m ()
dump = do
    say . showMap =<< use locus
    ctx <- use context
    say (show $ map (rootHash . _tzcFrom) ctx)

-- | Materialise tree node at locus and give it to action for introspection.
--
--   Often used with lambda-case: @withLocus $ \case { ... }@
withLocus :: Retrieves h k v m => (MapLayer h k v (Map h k v) -> Zipped h k v m a) -> Zipped h k v m a
withLocus action = do
    loc   <- use locus
    layer <- load loc
    action layer

-- | Add current node identity to the set of nodes touched.
mark :: Retrieves h k v m => String -> Zipped h k v m ()
mark _msg = do
    hash <- use locus <&> rootHash
    trail %= Set.insert hash

-- | Add given node identities to the set of nodes touched.
markAll :: Retrieves h k v m => [h] -> Zipped h k v m ()
markAll hashes = do
    trail %= (<> Set.fromList hashes)

data AlreadyOnTop = AlreadyOnTop deriving (Show, Typeable)

instance Exception AlreadyOnTop

-- | Move to the parent node; update & rebalance it if required.
up :: forall h k m v . Retrieves h k v m => Zipped h k v m Side
up = do
    dirty <- use tzDirty
    TreeZipperCtx side tree range wasDirty <- pop context AlreadyOnTop
    keyRange .= range   -- restore parent 'keyRange'
    load tree >>= \case
      MLBranch {_mlLeft = left, _mlRight = right, _mlTilt = tilt0} -> do
        if not dirty
        then do
            locus   .= tree
            tzDirty .= (dirty || wasDirty)

        else do
            now    <- use locus
            became <- case side of
              L -> do
                tilt'  <- correctTilt left now tilt0 L
                branch tilt' now right

              R -> do
                tilt'  <- correctTilt right now tilt0 R
                branch tilt' left now

            replaceWith became
            rebalance

      _other -> do
        throwM AlreadyOnTop

    return side

pop :: (Exception e, Retrieves h k v m) => Lens' (TreeZipper h k v) [a] -> e -> Zipped h k v m a
pop lens e = do
    list <- use lens
    case list of
        [] -> throwM e
        x : xs -> do
            lens .= xs
            return x

-- | Return to the root node.
exit :: Retrieves h k v m => Zipped h k v m (Map h k v)
exit = uplift
  where
    uplift = do
        _ <- up
        uplift
      `catch` \AlreadyOnTop ->
        use locus

-- | Open the tree for iteration.
enter :: Mode -> Map h k v -> TreeZipper h k v
enter mode0 tree = TreeZipper
    { _tzContext  = []
    , _tzHere     = tree
    , _tzKeyRange = (minBound, maxBound)
    , _tzMode     = mode0
    , _tzTouched  = Set.empty
    , _tzDirty    = False
    }

data WentDownOnNonBranch h = WentDownOnNonBranch h deriving (Show, Typeable)

instance (Show h, Typeable h) => Exception (WentDownOnNonBranch h)

-- | Move into the left branch of the current node.
descentLeft :: forall h k v m . Retrieves h k v m => Zipped h k v m ()
descentLeft = do
    tree  <- use locus
    range <- use keyRange

    mark "descentLeft"

    load tree >>= \case
      MLBranch { _mlLeft = left, _mlCenterKey = center } -> do
        locus    .= left
        dirty    <- use tzDirty
        context  %= (TreeZipperCtx L tree range dirty :)
        keyRange .= refine L range center
        tzDirty  .= False

        mark "descentLeft/exit"

      _layer -> do
        throwM $ WentDownOnNonBranch (rootHash tree)

-- | Move into the right branch of the current node.
descentRight :: Retrieves h k v m => Zipped h k v m ()
descentRight = do
    tree  <- use locus
    range <- use keyRange

    mark "descentRight"

    load tree >>= \case
      MLBranch { _mlRight = right, _mlCenterKey = center } -> do
        locus    .= right
        dirty    <- use tzDirty
        context  %= (TreeZipperCtx R tree range dirty :)
        keyRange .= refine R range center
        tzDirty  .= False

        mark "descentRight/exit"

      _layer -> do
        throwM $ WentDownOnNonBranch (rootHash tree)

-- | Using side and current 'centerKey', select a key subrange we end in.
refine
    :: Ord key
    => Side
    -> (WithBounds key, WithBounds key)
    ->  key
    -> (WithBounds key, WithBounds key)
refine L (l, h) (Plain -> m) = (l, min m h)
refine R (l, h) (Plain -> m) = (max m l, h)

-- | Correct tilt.
correctTilt :: Retrieves h k v m => Map h k v -> Map h k v -> Tilt -> Side -> Zipped h k v m Tilt
correctTilt was became tilt0 side = do
    modus   <- use mode
    deeper  <- deepened  was became
    shorter <- shortened was became
    let
      -- If we inserted and tree became deeper,  increase tilt to that side.
      -- if we deleted  and tree became shorter, decrease tilt to that side.
      res = case modus of
        UpdateMode | deeper  -> roll tilt0 side
        DeleteMode | shorter -> roll tilt0 (another side)
        _                    -> tilt0

    return res

-- | Find if tree became deeper.
deepened :: Retrieves h k v m => Map h k v -> Map h k v -> Zipped h k v m Bool
deepened was became = do
    wasLayer    <- load was
    becameLayer <- load became
    wasTilt     <- tilt was
    becameTilt  <- tilt became

    case (wasLayer, becameLayer) of
      (MLLeaf   {}, MLBranch {}) -> return True
      (MLBranch {}, MLBranch {}) -> return $
            wasTilt      ==    M
        &&  becameTilt `elem` [L1, R1]

      _                          -> return False

-- | Find if tree became shorter.
shortened :: Retrieves h k v m => Map h k v -> Map h k v -> Zipped h k v m Bool
shortened was became = do
    wasLayer    <- load was
    becameLayer <- load became
    wasTilt     <- tilt was
    becameTilt  <- tilt became

    case (wasLayer, becameLayer) of
      (MLBranch {}, MLLeaf   {}) -> return True
      (MLBranch {}, MLBranch {}) -> return $
            becameTilt   ==    M
        &&  wasTilt    `elem` [L1, R1]

      _                          -> return False

-- | Change tilt depending on grown side.
roll :: Tilt -> Side -> Tilt
roll tilt0 side =
    case side of
      L -> pred tilt0
      R -> succ tilt0

-- | Perform a zipper action upon current node, then update set its revision
--   to be a new one.
change
    :: Retrieves h k v m
    => (Zipped h k v m a)
    -> Zipped h k v m a
change action = do
    modus <- use mode
    when (modus == ReadonlyMode) $ do
        error "change: calling this in ReadonlyMode is prohibited"

    mark "change" -- automatically add node the list of touched
    res <- action
    tzDirty .= True
    return res

-- | Place new tree into the locus, update its revision.
replaceWith :: Retrieves h k v m => Map h k v -> Zipped h k v m ()
replaceWith newTree = do
    change (locus .= newTree)

-- | Fix imbalances around current locus of editation.
rebalance :: forall h k v m . Retrieves h k v m => Zipped h k v m ()
rebalance = do
    tree <- use locus

    let hashes |- nodeGen = nodeGen <* markAll hashes

    let combine' fork left right = do
            l <- left
            r <- right
            fork l r

    newTree <- load tree >>= \case
      Node r1 L2 left d -> do
        load left >>= \case
          Node r2 L1 a b     -> [r1, r2]     |- combine' (branch M)  (pure a)        (branch M  b d)
          Node r2 M  a b     -> [r1, r2]     |- combine' (branch R1) (pure a)        (branch L1 b d)
          Node r2 R1 a right -> do
            load right >>= \case
              Node r3 R1 b c -> [r1, r2, r3] |- combine' (branch M)  (branch L1 a b) (branch M  c d)
              Node r3 L1 b c -> [r1, r2, r3] |- combine' (branch M)  (branch M  a b) (branch R1 c d)
              Node r3 M  b c -> [r1, r2, r3] |- combine' (branch M)  (branch M  a b) (branch M  c d)
              _              -> return tree

          _ -> return tree

      Node r1 R2 a right -> do
        load right >>= \case
          Node r2 R1 b c     -> [r1, r2]     |- combine' (branch M)  (branch M  a b) (pure c)
          Node r2 M  b c     -> [r1, r2]     |- combine' (branch L1) (branch R1 a b) (pure c)
          Node r2 L1 left d  -> do
            load left >>= \case
              Node r3 R1 b c -> [r1, r2, r3] |- combine' (branch M)  (branch L1 a b) (branch M  c d)
              Node r3 L1 b c -> [r1, r2, r3] |- combine' (branch M)  (branch M  a b) (branch R1 c d)
              Node r3 M  b c -> [r1, r2, r3] |- combine' (branch M)  (branch M  a b) (branch M  c d)
              _              -> return tree

          _ -> return tree

      _ -> return tree

    replaceWith newTree

gotoPrevKey :: forall h k v m . Retrieves h k v m => k -> Zipped h k v m ()
gotoPrevKey k = do
    goto (Plain k)
    raiseUntilFrom R `catch` \AlreadyOnTop -> return ()
    descentLeft      `catch` \(WentDownOnNonBranch (_ :: h)) -> return ()
    whilePossible descentRight

gotoNextKey :: forall h k v m . Retrieves h k v m => k -> Zipped h k v m ()
gotoNextKey k = do
    goto (Plain k)
    raiseUntilFrom L `catch` \AlreadyOnTop -> return ()
    descentRight     `catch` \(WentDownOnNonBranch (_ :: h)) -> return ()
    whilePossible descentLeft

raiseUntilFrom :: Retrieves h k v m => Side -> Zipped h k v m ()
raiseUntilFrom weSeek = aux
  where
    aux = do
        side <- up
        unless (side == weSeek) $ do
            aux

whilePossible :: Retrieves h k v m => Zipped h k v m a -> Zipped h k v m ()
whilePossible action = aux
  where
    aux = do
        possible <- do
            _ <- action
            return True
          `catch` \(_ :: SomeException) -> do
            return False

        when possible $ do
            aux

-- | Teleport to a 'Leaf' with given key from anywhere.
goto :: Retrieves h k v m => WithBounds k -> Zipped h k v m ()
goto k = do
    raiseUntilHaveInRange k
    descentOnto k

-- | Raise, until given key is in range.
raiseUntilHaveInRange :: Retrieves h k v m => WithBounds k -> Zipped h k v m ()
raiseUntilHaveInRange k = goUp
  where
    goUp = do
        range <- use keyRange
        unless (k `isInside` range) $ do
            _ <- up
            goUp

    k' `isInside` (l, h) = k' >= l && k' <= h

-- | Teleport to a 'Leaf' with given key from above.
descentOnto :: forall h k v m . Retrieves h k v m => WithBounds k -> Zipped h k v m ()
descentOnto key0 = continueDescent
  where
    continueDescent = do
        loc      <- use locus
        center   <- centerKey loc
        if key0 >= center
            then descentRight
            else descentLeft
        continueDescent
      `catch` \(WentDownOnNonBranch (_ :: h)) -> do
        return ()
