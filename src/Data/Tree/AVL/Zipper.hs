-- | This module represents zipper layer.
--   It allows us represent all operations using
--     'up', 'descentLeft', 'descentRight' and 'change'
--   primitives.
--   The 'up' also rebalances and recalculates a node, if you 'change'd it
--   or its sibling.

module Data.Tree.AVL.Zipper where

import           Control.Exception          (Exception)
import           Control.Lens               (Getter, Lens', makeLenses, use, uses, (%=),
                                             (.=))

import           Control.Monad              (unless, when)
import           Control.Monad.Catch        (catch, throwM)
import           Control.Monad.State.Strict (StateT, evalStateT, get, lift, put)

import           Data.Monoid                ((<>))
import           Data.Set                   (Set)
import           Data.Typeable              (Typeable)

--import Debug.Trace as Debug (trace)

import           Data.Tree.AVL.Internal
import           Data.Tree.AVL.Proof
import           Data.Tree.AVL.Prune

--import qualified Debug.Trace as Debug

import qualified Data.Set                   as Set (empty, fromList, insert)

-- | Zipper representation.
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
    { _tzContext  :: [TreeZipperCxt h k v]  -- penetrated layers
    , _tzHere     :: Map h k v              -- current point
    , _tzKeyRange :: (Range k)              -- the diapasone of keys below locus
    , _tzMode     :: Mode
    , _tzTouched  :: Set h                  -- set of nodes we touched
    }

type Range k = (WithBounds k, WithBounds k)

-- | Tree layers.
data TreeZipperCxt h k v
    = WentRightFrom
        (Map h k v)  -- the node we came from (parent)
        (Range k)    -- the key diapasone of parent
        h            -- previous revision of _current_ (AFAIR) node
    | WentLeftFrom  (Map h k v) (Range k) h
    | JustStarted                         h

--deriving instance Retrieves h k v => Show (TreeZipperCxt h k v)

data Mode
    = UpdateMode
    | DeleteMode
    | ReadonlyMode
    deriving (Show, Eq)

makeLenses ''TreeZipper

context  :: Lens'  (TreeZipper h k v) [TreeZipperCxt h k v]
locus    :: Lens'  (TreeZipper h k v) (Map h k v)
keyRange :: Lens'  (TreeZipper h k v) (Range k)
mode     :: Getter (TreeZipper h k v)  Mode
trail    :: Lens'  (TreeZipper h k v) (Set h)

mode     = tzMode
keyRange = tzKeyRange
locus    = tzHere
context  = tzContext
trail    = tzTouched

-- | The zipper is the state we maintain. Any operation can fail.
type Zipped h k v m = StateT (TreeZipper h k v) m

instance
    ( Monad m
    , KVRetrieve h (Isolated h k v) m
    )
  =>
    KVRetrieve h (Isolated h k v) (Zipped h k v m)
  where
    retrieve = lift . retrieve

-- | Run zipper operation, collect prefabs to build proof.
runZipped' :: Retrieves h k v m => Zipped h k v m a -> Mode -> Map h k v -> m (a, Map h k v, Set h)
runZipped' action mode0 tree = do
    zipper <- enter mode0 tree
    action' `evalStateT` zipper
  where
    action' = do
      res    <- action
      tree'  <- exit
      trails <- use trail
      return (res, tree', trails)

-- | Run zipper operation, collect proof.
runZipped :: Retrieves h k v m => Zipped h k v m a -> Mode -> Map h k v -> m (a, Map h k v, Proof h k v)
runZipped action mode0 tree = do
    zipper             <- enter mode0 tree
    (a, tree1, trails) <- action' `evalStateT` zipper
    return (a, tree1, prune trails tree)
  where
    action' = do
      res    <- action
      tree'  <- exit
      trails <- use trail
      return (res, tree', trails)

withLocus :: Retrieves h k v m => (MapLayer h k v (Map h k v) -> Zipped h k v m a) -> Zipped h k v m a
withLocus action = do
    loc   <- use locus
    layer <- open loc
    action layer

-- | Add current node identity to the set of nodes touched.
mark :: Retrieves h k v m => String -> Zipped h k v m ()
mark _msg = do
    hash <- uses locus rootHash
    trail %= Set.insert hash

-- | Add given node identities to the set of nodes touched.
markAll :: Retrieves h k v m => [h] -> Zipped h k v m ()
markAll revs = do
    trail %= (<> Set.fromList revs)

rehashLocus :: Retrieves h k v m => Zipped h k v m ()
rehashLocus = do
    locus %= rehash

data AlreadyOnTop = AlreadyOnTop deriving (Show, Typeable)

instance Exception AlreadyOnTop

-- | Move to the parent node; update & 'rebalance' it if required.
up :: forall h k m v . Retrieves h k v m => Zipped h k v m Side
up = do
    rehashLocus
    ctx   <- use context
    hash1 <- uses locus rootHash :: Zipped h k v m h
    side  <- case ctx of
      WentLeftFrom tree range hash0 : rest -> do
        open tree >>= \case
          MLBranch {_mlLeft = left, _mlRight = right, _mlTilt = tilt0} -> do
            became <- do
                if hash0 == hash1  -- if current node didn't change
                then do
                    return tree  -- return unchanged parent one

                else do
                    -- install current node inside parent
                    -- prepare it to 'rebalance'
                    now   <- use locus
                    tilt' <- correctTilt left now tilt0 L
                    branch tilt' now right

            context  .= rest    -- pop parent layer from context stack
            keyRange .= range   -- restore parent 'keyRange'

            replaceWith became  -- replace current node with possibly updated
                                -- parent
                                -- also, make parent dirty, so next 'up'
                                -- will check if it needs to update

            rebalance
            return L            -- return the side we went from

          _other -> do
            throwM AlreadyOnTop
            --isolated1 <- lift $ traverse rootHash _other
            --error $ "up: zipper is broken " ++ show isolated1

      WentRightFrom tree range hash0 : rest -> do
        open tree >>= \case
          MLBranch {_mlLeft = left, _mlRight = right, _mlTilt = tilt0} -> do
            became <- do
                if hash0 == hash1
                then do
                    return tree

                else do
                    now   <- use locus
                    tilt' <- correctTilt right now tilt0 R
                    branch tilt' left now

            context  .= rest
            keyRange .= range

            replaceWith became

            rebalance
            return R

          _other -> do
            throwM AlreadyOnTop
            --isolated1 <- lift $ traverse rootHash _other
            --liftIO $ print isolated1
            --error "up: zipper is broken"

      [JustStarted _rev0] -> do
          rebalance        -- TODO: investigate
          context .= []
          return L

      [] -> do
          throwM AlreadyOnTop

      _other -> do
        error "up: zipper is broken beyond repair"


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

-- | Open the tree.
-- | Acts like "Tree.rootIterator()" in Java.
enter :: Retrieves h k v m => Mode -> Map h k v -> m (TreeZipper h k v)
enter mode0 tree = do
  return TreeZipper
    { _tzContext  = [JustStarted (rootHash tree)]
    , _tzHere     = tree
    , _tzKeyRange = (minBound, maxBound)
    , _tzMode     = mode0
    , _tzTouched  = Set.empty
    }

data WentDownOnNonBranch h = WentDownOnNonBranch h deriving (Show, Typeable)

instance (Show h, Typeable h) => Exception (WentDownOnNonBranch h)

-- | Move into the left branch of the current node.
descentLeft :: forall h k v m . Retrieves h k v m => Zipped h k v m ()
descentLeft = do
    tree  <- use locus
    range <- use keyRange
    mark "descentLeft"
    open tree >>= \case
      MLBranch { _mlLeft = left, _mlCenterKey = center } -> do
          context  %= (WentLeftFrom tree range (rootHash left) :)
          locus    .= left
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
    open tree >>= \case
      MLBranch { _mlRight = right, _mlCenterKey = center } -> do
          context  %= (WentRightFrom tree range (rootHash right) :)
          locus    .= right
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
        _          -> tilt0

    return res

-- | Find if tree became deeper.
deepened :: Retrieves h k v m => Map h k v -> Map h k v -> Zipped h k v m Bool
deepened was became = do
    wasLayer    <- open was
    becameLayer <- open became
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
    wasLayer    <- open was
    becameLayer <- open became
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
    action

replaceWith :: Retrieves h k v m => Map h k v -> Zipped h k v m ()
replaceWith newTree = do
    change (locus .= newTree)

--materializeTop3 :: Map h k v m -> m (MapLayer h k v (MapLayer h k v (MapLayer h k v h)))
--materializeTop3 tree = do
--  layer <- open tree


--dematerializeTop3 :: MapLayer h k v (MapLayer h k v (MapLayer h k v h)) -> Map h k v m

rebalance :: forall h k v m . Retrieves h k v m => Zipped h k v m ()
rebalance = do
    tree <- use locus

    let
      --(|-) :: [Revision] -> m (Map h k v) -> m ([Revision], Map h k v)
      hashes |- nodeGen = do
        gen <- nodeGen
        return (hashes, gen)

    let
      combine fork left right = do
        l <- left
        r <- right
        fork l r

    --wasGood <- isBalancedToTheLeaves tree

    (revs, newTree) <- (open tree >>= \case
      Node r1 L2 left d -> do
        open left >>= \case
          Node r2 L1 a b     -> [r1, r2]     |- combine (branch M)  (pure a)        (branch M  b d)
          Node r2 M  a b     -> [r1, r2]     |- combine (branch R1) (pure a)        (branch L1 b d)
          Node r2 R1 a right -> do
            open right >>= \case
              Node r3 R1 b c -> [r1, r2, r3] |- combine (branch M)  (branch L1 a b) (branch M  c d)
              Node r3 L1 b c -> [r1, r2, r3] |- combine (branch M)  (branch M  a b) (branch R1 c d)
              Node r3 M  b c -> [r1, r2, r3] |- combine (branch M)  (branch M  a b) (branch M  c d)
              _              -> return ([], tree)

          _ -> return ([], tree)

      Node r1 R2 a right -> do
        open right >>= \case
          Node r2 R1 b c     -> [r1, r2]     |- combine (branch M)  (branch M  a b) (pure c)
          Node r2 M  b c     -> [r1, r2]     |- combine (branch L1) (branch R1 a b) (pure c)
          Node r2 L1 left d  -> do
            open left >>= \case
              Node r3 R1 b c -> [r1, r2, r3] |- combine (branch M)  (branch L1 a b) (branch M  c d)
              Node r3 L1 b c -> [r1, r2, r3] |- combine (branch M)  (branch M  a b) (branch R1 c d)
              Node r3 M  b c -> [r1, r2, r3] |- combine (branch M)  (branch M  a b) (branch M  c d)
              _              -> return ([], tree)

          _ -> return ([], tree)

      _ -> return ([], tree))
     `catch` \(NotFound (_ :: h)) ->
        return ([], tree)

    markAll revs

    --isGood <- isBalancedToTheLeaves newTree

    --liftIO $ when (not isGood) $ do
    --    putStrLn $ "WAS " ++ showMap tree
    --    putStrLn $ "NOW " ++ showMap newTree

    replaceWith newTree

-- | Was used to track proofs, now obsolete.
--   TODO: remove.
separately :: Retrieves h k v m => Zipped h k v m a -> Zipped h k v m a
separately action = do
    state0 <- get
    result <- action
    put state0
    return result

--track :: Show a => String -> a -> Zipped h k v m ()
--track msg val = do
--    Debug.trace (msg <> " " <> show val) $ return ()

-- | Teleport to a 'Leaf' with given key from anywhere.
goto :: Retrieves h k v m => WithBounds k -> Zipped h k v m ()
goto key0 = do
    raiseUntilHaveInRange key0
    descentOnto key0

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
      `catch` \(WentDownOnNonBranch (_ :: h)) -> do
        return ()
