
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

-- | This module represents zipper layer.
--   It allows us represent all operations using
--     'up', 'descentLeft', 'descentRight' and 'change'
--   primitives.
--   The 'up' also rebalances and recalculates a node, if you 'change'd it
--   or its sibling.

module Data.Tree.AVL.Zipper where

import Control.Applicative ((<|>))
import Control.Lens (Getter, Lens', makeLenses, use, (%=), (+=), (.=), (^.))

import Control.Monad (unless, when)
import Control.Monad.State.Strict (StateT, evalStateT, get, put)

import Data.Monoid ((<>))
import Data.Set (Set)

import Debug.Trace as Debug (trace)

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
data TreeZipper h k v = TreeZipper
    { _tzContext  :: [TreeZipperCxt h k v]  -- penetrated layers
    , _tzHere     :: Map h k v              -- current point
    , _tzKeyRange :: (k, k)                 -- the diapasone of keys below locus
    , _tzMode     :: Mode
    , _tzRevision :: Revision               -- the incremented counter for rev
    , _tzTouched  :: RevSet                 -- set of nodes we touched
    }

type RevSet = Set Revision

-- | Tree layers.
data TreeZipperCxt h k v
    = WentRightFrom
        (Map h k v)  -- the node we came from (parent)
        (k, k)       -- the key diapasone of parent
        Revision     -- previous revision of _current_ (AFAIR) node
    | WentLeftFrom  (Map h k v) (k, k) Revision
    | JustStarted                      Revision

deriving instance Hash h k v => Show (TreeZipperCxt h k v)

data Mode
    = UpdateMode
    | DeleteMode
    | ReadonlyMode
    deriving (Show, Eq)

makeLenses ''TreeZipper

context  :: Lens'  (TreeZipper h k v) [TreeZipperCxt h k v]
locus    :: Lens'  (TreeZipper h k v) (Map h k v)
keyRange :: Lens'  (TreeZipper h k v) (k, k)
mode     :: Getter (TreeZipper h k v)  Mode
trail    :: Lens'  (TreeZipper h k v) (Set Revision)

mode     = tzMode
keyRange = tzKeyRange
locus    = tzHere
context  = tzContext
trail    = tzTouched

instance HasRevision (TreeZipper h k v) where
    revision = tzRevision

-- | The zipper is the state we maintain. Any operation can fail.
type Zipped h k v = StateT (TreeZipper h k v) Maybe

-- | Run zipper operation, collect prefabs to build proof.
runZipped' :: Hash h k v => Zipped h k v a -> Mode -> Map h k v -> (a, Map h k v, RevSet)
runZipped' action mode0 tree =
    case
        action' `evalStateT` enter mode0 tree
    of
      Just it -> it
      Nothing -> error "runZipped': failed"
  where
    action' = do
      res    <- action
      tree'  <- exit
      trails <- use trail
      return (res, tree', trails)

-- | Run zipper operation, collect proof.
runZipped :: Hash h k v => Zipped h k v a -> Mode -> Map h k v -> (a, Map h k v, Proof h k v)
runZipped action mode0 tree = case mResult of
    Just (a, tree1, trails) ->
      (a, tree1, prune trails tree)
    Nothing ->
      error "runZipped: failed"
  where
    mResult = action' `evalStateT` enter mode0 tree

    action' = do
      res    <- action
      tree'  <- exit
      trails <- use trail
      return (res, tree', trails)

-- | Generate fresh revision.
newRevision :: Zipped h k v Revision
newRevision = do
    rev <- use revision
    revision += 1
    return rev

-- | For debug.
dump :: Hash h k v => String -> Zipped h k v ()
dump msg = do
    st <- use context
    track ("==== " <> msg <> " BEGIN ====") ()
    track ("ctx\n") st
    l  <- use locus
    track ("lcs\n") l
    track ("==== " <> msg <> " END ====") ()
    return ()

-- | Add current node identity to the set of nodes touched.
mark :: Zipped h k v ()
mark = do
    rev0 <- use (locus.revision)
    trail %= Set.insert rev0

-- | Add given node identities to the set of nodes touched.
markAll :: [Revision] -> Zipped h k v ()
markAll revs = do
    trail %= (<> Set.fromList revs)

-- | Move to the parent node; update & 'rebalance' it if required.
up :: Hash h k v => Zipped h k v Side
up = do
    ctx  <- use context
    rev1 <- use (locus.revision)  -- retrive actual 'revision' of current node
    side <- case ctx of
      WentLeftFrom tree range rev0 : rest
        | Just fork <- tree^.branching -> do
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
                  tilt' <- correctTilt (fork^.left) now (fork^.tilt) L
                  let b = branch rev' tilt' now (fork^.right)
                  return b

          context  .= rest    -- pop parent layer from context stack
          keyRange .= range   -- restore parent 'keyRange'

          replaceWith became  -- replace current node with possibly updated
                              -- parent
                              -- also, make parent dirty, so next 'up'
                              -- will check if it needs to update

          rebalance
          return L            -- return the side we went from

      WentRightFrom tree range rev0 : rest
        | Just fork <- tree^.branching -> do
          became <- do
              if rev0 == rev1
              then do
                  return tree

              else do
                  locus %= rehash
                  now   <- use locus
                  rev'  <- newRevision
                  tilt' <- correctTilt (fork^.right) now (fork^.tilt) R
                  let b = branch rev' tilt' (fork^.left) now
                  return b

          context  .= rest
          keyRange .= range

          replaceWith became

          rebalance
          return R

      [JustStarted _rev0] -> do
          locus %= rehash  -- removing this causes tests to fail
          rebalance        -- TODO: investigate
          context .= []
          return L

      [] -> do
          fail "already on top"

      other -> do
        error $ "up: " ++ show other

    return side

-- | Return to the root node.
exit :: Hash h k v => Zipped h k v (Map h k v)
exit = uplift
  where
    uplift = do
        _ <- up
        uplift
      <|> use locus

-- | Open the tree.
-- | Acts like "Tree.rootIterator()" in Java.
enter :: Hash h k v => Mode -> Map h k v -> TreeZipper h k v
enter mode0 tree = TreeZipper
    { _tzContext  = [JustStarted (tree^.revision)]
    , _tzHere     = tree
    , _tzKeyRange = (minBound, maxBound)
    , _tzMode     = mode0
    , _tzRevision = tree^.revision + 1
    , _tzTouched  = Set.empty
    }

-- | Move into the left branch of the current node.
descentLeft :: Hash h k v => Zipped h k v ()
descentLeft = do
    tree  <- use locus
    range <- use keyRange
    mark
    case tree of
      _ | Just fork <- tree^.branching -> do
          let rev   = fork^.left.revision
          context  %= (WentLeftFrom tree range rev :)
          locus    .= fork^.left
          keyRange .= refine L range (tree^.centerKey)

        | otherwise -> do
            fail "cant' go down on non-branch"

-- | Move into the right branch of the current node.
descentRight :: Hash h k v => Zipped h k v ()
descentRight = do
    tree  <- use locus
    range <- use keyRange
    mark
    case tree of
      _ | Just fork <- tree^.branching -> do
          let rev = fork^.right.revision
          context  %= (WentRightFrom tree range rev :)
          locus    .= fork^.right
          keyRange .= refine R range (tree^.centerKey)

        | otherwise -> do
            fail "cant' go down on non-branch"

-- | Using side and current 'centerKey', select a key subrange we end in.
refine :: Ord key => Side -> (key, key) -> key -> (key, key)
refine L (l, h) m = (l, min m h)
refine R (l, h) m = (max m l, h)

-- | Correct tilt.
correctTilt :: Hash h k v => Map h k v -> Map h k v -> Tilt -> Side -> Zipped h k v Tilt
correctTilt was became tilt0 side = do
    modus <- use mode
    let
      -- If we inserted and tree became deeper,  increase tilt to that side.
      -- if we deleted  and tree became shorter, decrease tilt to that side.
      res = case modus of
        UpdateMode | deepened  was became -> roll tilt0 side
        DeleteMode | shortened was became -> roll tilt0 (another side)
        _          -> tilt0

    return res

-- | Find if tree became deeper.
deepened :: Map h k v -> Map h k v -> Bool
deepened was became
  | Just wasF    <- was   ^.branching
  , Just becameF <- became^.branching
    =   wasF   ^.tilt   ==    M
    &&  becameF^.tilt `elem` [L1, R1]
deepened Leaf{} Branch{} = True
deepened _      _        = False

-- | Find if tree became shorter.
shortened :: Map h k v -> Map h k v -> Bool
shortened was became
  | Just wasF    <- was   ^.branching
  , Just becameF <- became^.branching
    =   wasF   ^.tilt `elem` [L1, R1]
    &&  becameF^.tilt   ==    M
shortened Branch{} Leaf{} = True
shortened _        _      = False

-- | Change tilt depending on grown side.
roll :: Tilt -> Side -> Tilt
roll tilt0 side =
    case side of
      L -> pred tilt0
      R -> succ tilt0

-- | Perform a zipper action upon current node, then update set its revision
--   to be a new one.
change
    :: Hash h k v
    => (Zipped h k v a)
    -> Zipped h k v a
change action = do
    modus <- use mode
    when (modus == ReadonlyMode) $ do
        error "change: calling this in ReadonlyMode is prohibited"

    mark  -- automatically add node the list of touched
    res <- action
    rev <- newRevision
    locus.revision .= rev
    return res

replaceWith :: Hash h k v => Map h k v -> Zipped h k v ()
replaceWith newTree = do
    change (locus .= newTree)

rebalance :: Hash h k v => Zipped h k v ()
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

    newTree <- case tree of
      Node r1 L2 (Node r2 L1 a b) c -> do
        markAll [r1, r2]
        return $ node1 a (node2 b c)

      Node r1 R2 a (Node r2 R1 b c) -> do
        markAll [r1, r2]
        return $ node1 (node2 a b) c

      Node r1 R2 a (Node r2 M b c) -> do
        markAll [r1, r2]
        return $ skewn2 L1 (skewn3 R1 a b) c

      Node r1 L2 (Node r2 M a b) c -> do
        markAll [r1, r2]
        return $ skewn2 R1 a (skewn3 L1 b c)

      Node r1 L2 (Node r2 R1 a (Node r3 R1 b c)) d -> do
        markAll [r1, r2, r3]
        return $ node1 (skewn2 L1 a b) (node3 c d)

      Node r1 L2 (Node r2 R1 a (Node r3 L1 b c)) d -> do
        markAll [r1, r2, r3]
        return $ node1 (node2 a b) (skewn3 R1 c d)

      Node r1 L2 (Node r2 R1 a (Node r3 M  b c)) d -> do
        markAll [r1, r2, r3]
        return $ node1 (node2 a b) (node3 c d)

      Node r1 R2 a (Node r2 L1 (Node r3 R1 b c) d) -> do
        markAll [r1, r2, r3]
        return $ node1 (skewn2 L1 a b) (node2 c d)

      Node r1 R2 a (Node r2 L1 (Node r3 L1 b c) d) -> do
        markAll [r1, r2, r3]
        return $ node1 (node2 a b) (skewn3 R1 c d)

      Node r1 R2 a (Node r2 L1 (Node r3 M  b c) d) -> do
        markAll [r1, r2, r3]
        return $ node1 (node2 a b) (node3 c d)

      other ->
        return other

    replaceWith newTree

-- | Was used to track proofs, now obsolete.
--   TODO: remove.
separately :: Zipped h k v a -> Zipped h k v a
separately action = do
    state0 <- get
    result <- action
    put state0
    return result

track :: Show a => String -> a -> Zipped h k v ()
track msg val = do
    Debug.trace (msg <> " " <> show val) $ return ()

-- | Teleport to a 'Leaf' with given key from anywhere.
goto :: Hash h k v => k -> Zipped h k v ()
goto key0 = do
    raiseUntilHaveInRange key0
    descentOnto key0

raiseUntilHaveInRange :: Hash h k v => k -> Zipped h k v ()
raiseUntilHaveInRange key0 = goUp
  where
    goUp = do
        range <- use keyRange
        unless (key0 `isInside` range) $ do
            _ <- up
            goUp

    k `isInside` (l, h) = k >= l && k <= h

-- | Teleport to a 'Leaf' with given key from above.
descentOnto :: Hash h k v => k -> Zipped h k v ()
descentOnto key0 = continueDescent
  where
    continueDescent = do
        center <- use (locus.centerKey)
        if key0 >= center
        then descentRight
        else descentLeft
        continueDescent
      <|> return ()
