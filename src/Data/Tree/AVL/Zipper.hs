
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE ScopedTypeVariables  #-}

-- | This module represents zipper layer.
--   It allows us represent all operations using
--     'up', 'descentLeft', 'descentRight' and 'change'
--   primitives.
--   The 'up' also rebalances and recalculates a node, if you 'change'd it
--   or its sibling.

module Data.Tree.AVL.Zipper where

import Control.Applicative ((<|>))
import Control.Exception(Exception)
import Control.Lens (Getter, Lens', makeLenses, use, (%=), (.=), (^.), (.~))

import Control.Monad (unless, when)
import Control.Monad.Catch (throwM)
import Control.Monad.State.Strict (StateT, evalStateT, get, put, modify, lift)

import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Typeable (Typeable)

--import Debug.Trace as Debug (trace)

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof
import Data.Tree.AVL.Prune

import qualified Data.Set as Set

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
data TreeZipper h k v m = TreeZipper
    { _tzContext  :: [TreeZipperCxt h k v m]  -- penetrated layers
    , _tzHere     :: Map h k v m              -- current point
    , _tzKeyRange :: (k, k)                   -- the diapasone of keys below locus
    , _tzMode     :: Mode
    , _tzRevision :: Revision                 -- the incremented counter for rev
    , _tzTouched  :: RevSet                   -- set of nodes we touched
    }

type RevSet = Set Revision

-- | Tree layers.
data TreeZipperCxt h k v m
    = WentRightFrom
        (Map h k v m)  -- the node we came from (parent)
        (k, k)         -- the key diapasone of parent
        Revision       -- previous revision of _current_ (AFAIR) node
    | WentLeftFrom  (Map h k v m) (k, k) Revision
    | JustStarted                        Revision

--deriving instance Stores h k v => Show (TreeZipperCxt h k v)

data Mode
    = UpdateMode
    | DeleteMode
    | ReadonlyMode
    deriving (Show, Eq)

makeLenses ''TreeZipper

context  :: Lens'  (TreeZipper h k v m) [TreeZipperCxt h k v m]
locus    :: Lens'  (TreeZipper h k v m) (Map h k v m)
keyRange :: Lens'  (TreeZipper h k v m) (k, k)
mode     :: Getter (TreeZipper h k v m)  Mode
trail    :: Lens'  (TreeZipper h k v m) (Set Revision)

mode     = tzMode
keyRange = tzKeyRange
locus    = tzHere
context  = tzContext
trail    = tzTouched

instance Monad m => HasRevision m (TreeZipper h k v m) where
    revision zipper = return (zipper^.tzRevision)
    setRevision r   = tzRevision .~ r

-- | The zipper is the state we maintain. Any operation can fail.
type Zipped h k v m = StateT (TreeZipper h k v m) m

-- | Run zipper operation, collect prefabs to build proof.
runZipped' :: Stores h k v m => Zipped h k v m a -> Mode -> Map h k v m -> m (a, Map h k v m, RevSet)
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
runZipped :: Stores h k v m => Zipped h k v m a -> Mode -> Map h k v m -> m (a, Map h k v m, Proof h k v)
runZipped action mode0 tree = do
    zipper             <- enter mode0 tree
    (a, tree1, trails) <- action' `evalStateT` zipper
    proof              <- prune trails tree
    return (a, tree1, proof)
  where
    action' = do
      res    <- action
      tree'  <- exit
      trails <- use trail
      return (res, tree', trails)

-- | Generate fresh revision.
newRevision :: Monad m => Zipped h k v m Revision
newRevision = do
    st  <- get
    rev <- lift $ revision st
    modify (setRevision (rev + 1))
    return rev

---- | For debug.
--dump :: Stores h k v m => String -> Zipped h k v m ()
--dump msg = do
--    st <- use context
--    track ("==== " <> msg <> " BEGIN ====") ()
--    track ("ctx\n") st
--    l  <- use locus
--    track ("lcs\n") l
--    track ("==== " <> msg <> " END ====") ()
--    return ()

-- | Add current node identity to the set of nodes touched.
mark :: Stores h k v m => Zipped h k v m ()
mark = do
    tree <- use locus
    rev0 <- lift $ revision tree
    trail %= Set.insert rev0

-- | Add given node identities to the set of nodes touched.
markAll :: Stores h k v m => [Revision] -> Zipped h k v m ()
markAll revs = do
    trail %= (<> Set.fromList revs)

data AlreadyOnTop = AlreadyOnTop deriving (Show, Typeable)

instance Exception AlreadyOnTop

-- | Move to the parent node; update & 'rebalance' it if required.
up :: forall h k m v . Stores h k v m => Zipped h k v m Side
up = do
    ctx  <- use context
    loc  <- use locus :: Zipped h k v m (Map h k v m)
    rev1 <- lift $ revision loc -- retrive actual 'revision' of current node
    side <- case ctx of
      WentLeftFrom tree range rev0 : rest -> do
        layer <- lift $ pick loc
        case layer of
          MLBranch {_mlLeft = left, _mlRight = right, _mlTilt = tilt0} -> do
            became <- do
                if rev0 == rev1  -- if current node didn't 'change'
                then do
                    return tree  -- return unchanged parent one

                else do
                    -- install current node inside parent
                    -- prepare it to 'rebalance'
                    locus %= rehash
                    now   <- use locus
                    rev'  <- newRevision
                    tilt' <- correctTilt left now tilt0 L
                    let b = branch rev' tilt' now right
                    return b

            context  .= rest    -- pop parent layer from context stack
            keyRange .= range   -- restore parent 'keyRange'

            replaceWith became  -- replace current node with possibly updated
                                -- parent
                                -- also, make parent dirty, so next 'up'
                                -- will check if it needs to update

            rebalance
            return L            -- return the side we went from

          _ ->
            error "up: zipper is broken"

      WentRightFrom tree range rev0 : rest -> do
        layer <- lift $ pick loc
        case layer of
          MLBranch {_mlLeft = left, _mlRight = right, _mlTilt = tilt0} -> do
            became <- do
                if rev0 == rev1
                then do
                    return tree

                else do
                    locus %= rehash
                    now   <- use locus
                    rev'  <- newRevision
                    tilt' <- correctTilt right now tilt0 R
                    let b = branch rev' tilt' left now
                    return b

            context  .= rest
            keyRange .= range

            replaceWith became

            rebalance
            return R

          _ ->
            error "up: zipper is broken"

      [JustStarted _rev0] -> do
          locus %= rehash  -- removing this causes tests to fail
          rebalance        -- TODO: investigate
          context .= []
          return L

      [] -> do
          throwM AlreadyOnTop

      _other -> do
          error $ "up: zipper is broken"

    return side

-- | Return to the root node.
exit :: Stores h k v m => Zipped h k v m (Map h k v m)
exit = uplift
  where
    uplift = do
        _ <- up
        uplift
      <|> use locus

-- | Open the tree.
-- | Acts like "Tree.rootIterator()" in Java.
enter :: Stores h k v m => Mode -> Map h k v m -> m (TreeZipper h k v m)
enter mode0 tree = do
  rev <- revision tree
  return TreeZipper
    { _tzContext  = [JustStarted rev]
    , _tzHere     = tree
    , _tzKeyRange = (minBound, maxBound)
    , _tzMode     = mode0
    , _tzRevision = rev + 1
    , _tzTouched  = Set.empty
    }

data WentDownOnNonBranch h = WentDownOnNonBranch h deriving (Show, Typeable)

instance (Show h, Typeable h) => Exception (WentDownOnNonBranch h)

-- | Move into the left branch of the current node.
descentLeft :: Stores h k v m => Zipped h k v m ()
descentLeft = do
    tree  <- use locus
    range <- use keyRange
    mark
    layer <- lift $ pick tree
    case layer of
      MLBranch { _mlLeft = left, _mlCenterKey } -> do
          rev      <- lift $ revision left
          context  %= (WentLeftFrom tree range rev :)
          locus    .= left
          keyRange .= refine L range _mlCenterKey

      _ -> do
          throwM $ WentDownOnNonBranch (_mlHash layer)

-- | Move into the right branch of the current node.
descentRight :: Stores h k v m => Zipped h k v m ()
descentRight = do
    tree  <- use locus
    range <- use keyRange
    mark
    layer <- lift $ pick tree
    case layer of
      MLBranch { _mlRight = right, _mlCenterKey } -> do
          rev      <- lift $ revision right
          context  %= (WentRightFrom tree range rev :)
          locus    .= right
          keyRange .= refine R range _mlCenterKey

      _ -> do
          throwM $ WentDownOnNonBranch (_mlHash layer)

-- | Using side and current 'centerKey', select a key subrange we end in.
refine :: Ord key => Side -> (key, key) -> key -> (key, key)
refine L (l, h) m = (l, min m h)
refine R (l, h) m = (max m l, h)

-- | Correct tilt.
correctTilt :: Stores h k v m => Map h k v m -> Map h k v m -> Tilt -> Side -> Zipped h k v m Tilt
correctTilt was became tilt0 side = do
    modus   <- use mode
    deeper  <- lift $ deepened  was became
    shorter <- lift $ shortened was became
    let
      -- If we inserted and tree became deeper,  increase tilt to that side.
      -- if we deleted  and tree became shorter, decrease tilt to that side.
      res = case modus of
        UpdateMode | deeper  -> roll tilt0 side
        DeleteMode | shorter -> roll tilt0 (another side)
        _          -> tilt0

    return res

-- | Find if tree became deeper.
deepened :: Stores h k v m => Map h k v m -> Map h k v m -> m Bool
deepened was became = do
    wasLayer    <- pick was
    becameLayer <- pick became
    wasTilt     <- tilt was
    becameTilt  <- tilt became

    case (wasLayer, becameLayer) of
      (MLLeaf   {}, MLBranch {}) -> return True
      (MLBranch {}, MLBranch {}) -> return $
            wasTilt      ==    M
        &&  becameTilt `elem` [L1, R1]
      
      _                          -> return False

-- | Find if tree became shorter.
shortened :: Stores h k v m => Map h k v m -> Map h k v m -> m Bool
shortened was became = do
    wasLayer    <- pick was
    becameLayer <- pick became
    wasTilt     <- tilt was
    becameTilt  <- tilt became

    case (wasLayer, becameLayer) of
      (MLLeaf   {}, MLBranch {}) -> return True
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
    :: Stores h k v m
    => (Zipped h k v m a)
    -> Zipped h k v m a
change action = do
    modus <- use mode
    when (modus == ReadonlyMode) $ do
        error "change: calling this in ReadonlyMode is prohibited"

    mark  -- automatically add node the list of touched
    res <- action
    rev <- newRevision
    locus %= setRevision rev
    return res

replaceWith :: Stores h k v m => Map h k v m -> Zipped h k v m ()
replaceWith newTree = do
    change (locus .= newTree)

--materializeTop3 :: Map h k v m -> m (MapLayer h k v (MapLayer h k v (MapLayer h k v h)))
--materializeTop3 tree = do
--  layer <- pick tree


--dematerializeTop3 :: MapLayer h k v (MapLayer h k v (MapLayer h k v h)) -> Map h k v m

rebalance :: forall h k v m . Stores h k v m => Zipped h k v m ()
rebalance = do
    tree <- use locus
    rev1 <- newRevision  -- up to 3 nodes could be touched
    rev2 <- newRevision
    rev3 <- newRevision

    let node1 = branch rev1 M  -- making prefabs for them
    let node2 = branch rev2 M
    let node3 = branch rev3 M
    let skewn2 = branch rev2
    let skewn3 = branch rev3

    layer <- lift $ pick tree :: Zipped h k v m (MapLayer h k v (Map h k v m))
    newTree <- case layer of
      Node r1 L2 left d -> do
        leftLayer <- lift $ pick (left :: Map h k v m)
        case leftLayer of
          Node r2 L1 a b -> do markAll [r1, r2]; return $ node1     a (node2     b d)
          Node r2 M  a b -> do markAll [r1, r2]; return $ skewn2 R1 a (skewn3 L1 b d)
          Node r2 R1 a right -> do
            rightLayer <- lift $ pick (right :: Map h k v m)
            case rightLayer of
              Node r3 R1 b c -> do markAll [r1, r2, r3]; return $ node1 (skewn2 L1 a b) (node3     c d)
              Node r3 L1 b c -> do markAll [r1, r2, r3]; return $ node1 (node2     a b) (skewn3 R1 c d)
              Node r3 M  b c -> do markAll [r1, r2, r3]; return $ node1 (node2     a b) (node3     c d)
              _              -> return tree

          _ -> return tree

      Node r1 R2 a right -> do
        rightLayer <- lift $ pick (right :: Map h k v m)
        case rightLayer of
          Node r2 R1 b c -> do markAll [r1, r2]; return $ node1     (node2     a b) c
          Node r2 M  b c -> do markAll [r1, r2]; return $ skewn2 L1 (skewn3 R1 a b) c
          Node r2 L1 left d -> do
            leftLayer <- lift $ pick (left :: Map h k v m)
            case leftLayer of
              Node r3 R1 b c -> do markAll [r1, r2, r3]; return $ node1 (skewn2 L1 a b) (node2     c d)
              Node r3 L1 b c -> do markAll [r1, r2, r3]; return $ node1 (node2     a b) (skewn3 R1 c d)
              Node r3 M  b c -> do markAll [r1, r2, r3]; return $ node1 (node2     a b) (node3     c d)
              _              -> return tree

          _ -> return tree

      _ -> return tree

    replaceWith newTree

-- | Was used to track proofs, now obsolete.
--   TODO: remove.
separately :: Stores h k v m => Zipped h k v m a -> Zipped h k v m a
separately action = do
    state0 <- get
    result <- action
    put state0
    return result

--track :: Show a => String -> a -> Zipped h k v m ()
--track msg val = do
--    Debug.trace (msg <> " " <> show val) $ return ()

-- | Teleport to a 'Leaf' with given key from anywhere.
goto :: Stores h k v m => k -> Zipped h k v m ()
goto key0 = do
    raiseUntilHaveInRange key0
    descentOnto key0

raiseUntilHaveInRange :: Stores h k v m => k -> Zipped h k v m ()
raiseUntilHaveInRange key0 = goUp
  where
    goUp = do
        range <- use keyRange
        unless (key0 `isInside` range) $ do
            _ <- up
            goUp

    k `isInside` (l, h) = k >= l && k <= h

-- | Teleport to a 'Leaf' with given key from above.
descentOnto :: Stores h k v m => k -> Zipped h k v m ()
descentOnto key0 = continueDescent
  where
    continueDescent = do
        loc <- use locus
        center <- lift $ centerKey loc
        if key0 >= center
        then descentRight
        else descentLeft
        continueDescent
      <|> return ()
