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
    , up
    , descentLeft
    , descentRight

      -- * Actions over iterator
    , withLocus
    , change
    , replaceWith
    , freshRevision
    , mark

      -- * Current point iterator is standing on
    , locus
    )
  where

import Control.Exception (Exception)
import Control.Lens (Getter, Lens', makeLenses, use, (%=), (.=), (^.))

import Control.Monad (unless, when)
import Control.Monad.Catch (catch, throwM)
import Control.Monad.State.Strict (StateT, evalStateT, lift)

import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Typeable (Typeable)

--import Debug.Trace as Debug (trace)

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
    { _tzContext  :: [TreeZipperCxt h k v]  -- ^ Layers above us
    , _tzHere     ::  Map h k v             -- ^ Current point
    , _tzKeyRange :: (Range k)              -- ^ Range of keys covered by locus
    , _tzMode     ::  Mode                  -- ^ Update or delete
    , _tzTouched  ::  Set Revision          -- ^ Set of nodes we touched
    , _tzCounter  ::  Revision              -- ^ UID
    }

type Range k = (WithBounds k, WithBounds k)

-- | Tree layers.
data TreeZipperCxt h k v
    = WentRightFrom
        (Map h k v)  -- ^ the node we came from (parent)
        (Range k)    -- ^ the key diapasone of parent
         Revision    -- ^ previous revision of _current_ (AFAIR) node
    | WentLeftFrom
        (Map h k v)
        (Range k)
         Revision

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
context  :: Lens'  (TreeZipper h k v) [TreeZipperCxt h k v]
keyRange :: Lens'  (TreeZipper h k v) (Range k)
mode     :: Getter (TreeZipper h k v)  Mode
trail    :: Lens'  (TreeZipper h k v) (Set Revision)
counter  :: Lens'  (TreeZipper h k v)  Revision

mode     = tzMode
keyRange = tzKeyRange
locus    = tzHere
context  = tzContext
trail    = tzTouched
counter  = tzCounter

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
runZipped :: Retrieves h k v m => Zipped h k v m a -> Mode -> Map h k v -> m (a, Map h k v, Set Revision)
runZipped action mode0 tree = do
    zipper <- enter mode0 tree
    action' `evalStateT` zipper
  where
    action' = do
      res    <- action
      tree'  <- exit
      trails <- use trail
      return (res, tree', trails)

-- | Run zipper operation, build proof.
runZipped' :: Retrieves h k v m => Zipped h k v m a -> Mode -> Map h k v -> m (a, Map h k v, Proof h k v)
runZipped' action mode0 tree = do
    zipper             <- enter mode0 tree
    (a, tree1, trails) <- action' `evalStateT` zipper
    proof              <- prune trails (fullRehash tree)
    return (a, tree1, proof)
  where
    action' = do
      res    <- action
      tree'  <- exit
      trails <- use trail
      return (res, tree', trails)

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
    rev <- revisionHere
    trail %= Set.insert rev

revisionHere :: Retrieves h k v m => Zipped h k v m Revision
revisionHere = withLocus $ return . (^.mlRevision)

-- | Add given node identities to the set of nodes touched.
markAll :: Retrieves h k v m => [Revision] -> Zipped h k v m ()
markAll revs = do
    trail %= (<> Set.fromList revs)

-- rehashLocus :: Retrieves h k v m => Zipped h k v m ()
-- rehashLocus = do
--     locus %= rehash

data AlreadyOnTop = AlreadyOnTop deriving (Show, Typeable)

instance Exception AlreadyOnTop

-- | Move to the parent node; update & rebalance it if required.
up :: forall h k m v . Retrieves h k v m => Zipped h k v m Side
up = do
    -- rehashLocus
    ctx   <- use context
    rev1 <- revisionHere
    side  <- case ctx of
      WentLeftFrom tree range rev0 : rest -> do
        load tree >>= \case
          MLBranch {_mlLeft = left, _mlRight = right, _mlTilt = tilt0} -> do
            if rev0 == rev1  -- if current node didn't change
            then do
                locus .= tree  -- set unchanged parent one

            else do
                -- install current node inside parent
                -- prepare it to 'rebalance'
                now    <- use locus
                tilt'  <- correctTilt left now tilt0 L
                became <- branch rev1 tilt' now right

                replaceWith became  -- replace current node with possibly updated
                                    -- parent
                                    -- also, make parent dirty, so next 'up'
                                    -- will check if it needs to update
                rebalance

            context  .= rest    -- pop parent layer from context stack
            keyRange .= range   -- restore parent 'keyRange'

            return L            -- return the side we went from

          _other -> do
            throwM AlreadyOnTop
            --isolated1 <- lift $ traverse rootHash _other
            --error $ "up: zipper is broken " ++ show isolated1

      WentRightFrom tree range rev0 : rest -> do
        load tree >>= \case
          MLBranch {_mlLeft = left, _mlRight = right, _mlTilt = tilt0} -> do
            if rev0 == rev1
            then do
                locus .= tree

            else do
                now    <- use locus
                tilt'  <- correctTilt right now tilt0 R
                became <- branch rev1 tilt' left now

                replaceWith became
                rebalance

            context  .= rest
            keyRange .= range

            return R

          _other -> do
            throwM AlreadyOnTop
            --isolated1 <- lift $ traverse rootHash _other
            --liftIO $ print isolated1
            --error "up: zipper is broken"

      -- [JustStarted _rev0] -> do
      --     rebalance        -- TODO: investigate
      --     context .= []
      --     return L

      [] -> do
          throwM AlreadyOnTop

    return side

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
enter :: Retrieves h k v m => Mode -> Map h k v -> m (TreeZipper h k v)
enter mode0 tree = do
  rev <- loadAnd (^.mlRevision) tree
  return TreeZipper
    { _tzContext  = []
    , _tzHere     = tree
    , _tzKeyRange = (minBound, maxBound)
    , _tzMode     = mode0
    , _tzTouched  = Set.empty
    , _tzCounter  = rev
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
          locus .= left
          rev <- revisionHere
          context %= (WentLeftFrom tree range rev :)
          keyRange .= refine L range center
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
          locus .= right
          rev <- revisionHere
          context %= (WentRightFrom tree range rev :)
          keyRange .= refine R range center
          mark "descentRight/exit"

      _layer -> do
          throwM $ WentDownOnNonBranch (rootHash tree)

-- | Using side and current 'centerKey', select a key subrange we end in.
refine
    :: Ord key
    => Side
    -> (WithBounds key, WithBounds key)
    ->  WithBounds key
    -> (WithBounds key, WithBounds key)
refine L (l, h) m = (l, min m h)
refine R (l, h) m = (max m l, h)

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

-- printLocus :: Retrieves h k v m => String -> Zipped h k v m ()
-- printLocus str = do
--     tree     <- use locus
--     isolated <- isolate tree
--     liftIO $ print $ str ++ ": " ++ show isolated

-- dump :: Retrieves h k v m => String -> Zipped h k v m ()
-- dump str = do
--     tree     <- use locus
--     isolated <- isolate tree
--     ctx      <- use context
--     ls <- for ctx $ \case
--       WentRightFrom what krange rev -> do
--         res <- isolate what
--         return $ show (krange, rev) ++ " <- " ++ show res

--       WentLeftFrom what krange rev -> do
--         res <- isolate what
--         return $ show (krange, rev) ++ " -> " ++ show res

--       JustStarted rev -> do
--         return $ "start " ++ show rev

--     liftIO $ putStrLn $ str ++ ":\n  " ++ show isolated ++ "\n   --\n" ++ unlines (map ("  " ++) ls)

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
    a <- action

    rev    <- freshRevision
    loc    <- use locus
    newLoc <- lift $ setRevision rev loc

    locus .= newLoc

    return a

-- | Generate fresh, unique revision (per tree instance).
freshRevision :: Monad m => Zipped h k v m Revision
freshRevision = do
    counter %= (+ 1)
    use counter

-- | Place new tree into the locus, update its revision.
replaceWith :: Retrieves h k v m => Map h k v -> Zipped h k v m ()
replaceWith newTree = do
    change (locus .= newTree)

-- | Fix imbalances around current locus of editation.
rebalance :: forall h k v m . Retrieves h k v m => Zipped h k v m ()
rebalance = do
    tree <- use locus

    n1 <- freshRevision
    n2 <- freshRevision
    n3 <- freshRevision

    let
      revs |- nodeGen = do
        gen <- nodeGen
        return (revs, gen)

    let
      combine fork left right = do
        l <- left
        r <- right
        fork l r

    (revs, newTree) <- (load tree >>= \case
      Node r1 L2 left d -> do
        load left >>= \case
          Node r2 L1 a b     -> [r1, r2]     |- combine (branch n1 M)  (pure a)           (branch n2 M  b d)
          Node r2 M  a b     -> [r1, r2]     |- combine (branch n1 R1) (pure a)           (branch n3 L1 b d)
          Node r2 R1 a right -> do
            load right >>= \case
              Node r3 R1 b c -> [r1, r2, r3] |- combine (branch n1 M)  (branch n2 L1 a b) (branch n3 M  c d)
              Node r3 L1 b c -> [r1, r2, r3] |- combine (branch n1 M)  (branch n2 M  a b) (branch n3 R1 c d)
              Node r3 M  b c -> [r1, r2, r3] |- combine (branch n1 M)  (branch n2 M  a b) (branch n3 M  c d)
              _              -> return ([], tree)

          _ -> return ([], tree)

      Node r1 R2 a right -> do
        load right >>= \case
          Node r2 R1 b c     -> [r1, r2]     |- combine (branch n1 M)  (branch n2 M  a b) (pure c)
          Node r2 M  b c     -> [r1, r2]     |- combine (branch n1 L1) (branch n2 R1 a b) (pure c)
          Node r2 L1 left d  -> do
            load left >>= \case
              Node r3 R1 b c -> [r1, r2, r3] |- combine (branch n1 M)  (branch n2 L1 a b) (branch n3 M  c d)
              Node r3 L1 b c -> [r1, r2, r3] |- combine (branch n1 M)  (branch n2 M  a b) (branch n3 R1 c d)
              Node r3 M  b c -> [r1, r2, r3] |- combine (branch n1 M)  (branch n2 M  a b) (branch n3 M  c d)
              _              -> return ([], tree)

          _ -> return ([], tree)

      _ -> return ([], tree))
     `catch` \(NotFound (_ :: h)) ->
        return ([], tree)

    markAll revs

    replaceWith newTree

--track :: Show a => String -> a -> Zipped h k v m ()
--track msg val = do
--    Debug.trace (msg <> " " <> show val) $ return ()

-- | Teleport to a 'Leaf' with given key from anywhere.
goto :: Retrieves h k v m => WithBounds k -> Zipped h k v m ()
goto key0 = do
    raiseUntilHaveInRange key0
    descentOnto key0

-- | Raise, until given key is in range.
raiseUntilHaveInRange :: Retrieves h k v m => WithBounds k -> Zipped h k v m ()
raiseUntilHaveInRange key0 = goUp
  where
    goUp = do
        range <- use keyRange
        unless (key0 `isInside` range) $ do
            _ <- up
            goUp

    k `isInside` (l, h) = k >= l && k <= h

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
      `catch` \(WentDownOnNonBranch (_ :: Maybe h)) -> do
        return ()
