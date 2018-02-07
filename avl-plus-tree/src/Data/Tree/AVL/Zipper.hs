
{-# language NamedFieldPuns #-}
{-# language TemplateHaskell #-}

module Data.Tree.AVL.Zipper where

import Data.Monoid
import Data.Tree.AVL.Internal

import Debug.Trace as Debug

import Control.Applicative
import Control.Lens hiding (locus)
import Control.Monad.State.Strict

data TreeZipper h k v = TreeZipper
    { _tzContext  :: [TreeZipperCxt h k v]
    , _tzHere     :: Map h k v
    , _tzKeyRange :: (k, k)
    , _tzMode     :: Mode
    , _tzRevision :: Revision }

data TreeZipperCxt h k v
    = WentRightFrom (Map h k v) (k, k) Revision
    | WentLeftFrom  (Map h k v) (k, k) Revision
    | JustStarted
    deriving Show

data Mode
    = UpdateMode
    | DeleteMode
    | ReadonlyMode
    deriving (Show, Eq)

makeLenses ''TreeZipper

context :: Lens' (TreeZipper h k v) [TreeZipperCxt h k v]
context = tzContext

locus :: Lens' (TreeZipper h k v) (Map h k v)
locus = tzHere

keyRange :: Lens' (TreeZipper h k v) (k, k)
keyRange = tzKeyRange

mode :: Getter (TreeZipper h k v) Mode
mode = tzMode

instance HasRevision (TreeZipper h k v) where
    revision = tzRevision

type Zipped h k v = StateT (TreeZipper h k v) Maybe

runZipped :: Hash h k v => Zipped h k v a -> Mode -> Map h k v -> (a, Map h k v)
runZipped action mode tree = case mResult of
    Just it -> it
    Nothing -> error "runZipped: failed"
  where
    mResult = action' `evalStateT` enter mode tree

    action' = do
      res   <- action
      tree' <- exit
      return (res, tree')

newRevision :: Zipped h k v Revision
newRevision = do
    rev <- use revision
    revision += 1
    return rev

dump :: Hash h k v => String -> Zipped h k v ()
dump msg = do
    st <- use context
    track ("==== " <> msg <> " BEGIN ====") ()
    track ("ctx\n") st
    l  <- use locus
    track ("lcs\n") l
    track ("==== " <> msg <> " END ====") ()
    return ()

up :: Hash h k v => Zipped h k v Side
up = do
    ctx  <- use context
    rev1 <- use (locus.revision)
    side <- case ctx of
      WentLeftFrom tree @ Branch {} range rev0 : rest -> do
          became <- do
              if rev0 == rev1
              then do
                  return tree

              else do
                  locus %= rehash
                  now   <- use locus
                  rev'  <- newRevision
                  tilt' <- correctTilt (tree^?!aptLeft) now (tree^?!aptTilt) L
                  let b = branch rev' tilt' now (tree^?!aptRight)
                  return b

          context  .= rest
          keyRange .= range

          replaceWith became


          rebalance
          return L

      WentRightFrom tree @ Branch {} range rev0 : rest -> do
          became <- do
              if rev0 == rev1
              then do
                  return tree

              else do
                  locus %= rehash
                  now   <- use locus
                  rev'  <- newRevision
                  tilt' <- correctTilt (tree^?!aptRight) now (tree^?!aptTilt) R
                  let b = branch rev' tilt' (tree^?!aptLeft) now
                  return b

          context  .= rest
          keyRange .= range

          replaceWith became

          rebalance
          return R

      [JustStarted] -> do
          locus %= rehash
          rebalance
          context .= []
          return L

      [] -> do
          fail "already on top"

    return side

exit :: Hash h k v => Zipped h k v (Map h k v)
exit = uplift
  where
    uplift = do
        up
        uplift
      <|> use locus

enter :: Hash h k v => Mode -> Map h k v -> TreeZipper h k v
enter mode tree = TreeZipper
    { _tzContext  = [JustStarted]
    , _tzHere     = tree
    , _tzKeyRange = (minBound, maxBound)
    , _tzMode     = mode
    , _tzRevision = tree^.revision + 1
    }

descentLeft :: Hash h k v => Zipped h k v ()
descentLeft = do
    tree  <- use locus
    range <- use keyRange
    case tree of
      Branch {} -> do
        let rev = tree^?!aptLeft.revision
        context  %= (WentLeftFrom tree range rev :)
        locus    .= tree^?!aptLeft
        keyRange .= refine L range (tree^.centerKey)

      other -> do
          fail "cant' go down on non-branch"

descentRight :: Hash h k v => Zipped h k v ()
descentRight = do
    tree  <- use locus
    range <- use keyRange
    case tree of
      Branch {} -> do
        let rev = tree^?!aptRight.revision
        context  %= (WentRightFrom tree range rev :)
        locus    .= tree^?!aptRight
        keyRange .= refine R range (tree^.centerKey)

      other -> do
          fail "cant' go down on non-branch"

refine L (l, h) m = (l, min m h)
refine R (l, h) m = (max m l, h)

correctTilt :: Hash h k v => Map h k v -> Map h k v -> Tilt -> Side -> Zipped h k v Tilt
correctTilt was became tilt side = do
    modus <- use mode
    let
      res = case modus of
        UpdateMode | deepened  was became -> roll tilt side
        DeleteMode | shortened was became -> roll tilt (other side)
        _                                 -> tilt

    -- Debug.traceShow (tilt, res) $
    --  Debug.traceShow (height was, height became) $
    --  Debug.traceShow (modus, deepened was became) $
    --  Debug.traceShow ("was", was) $
    --  Debug.traceShow ("became", became) $
    return res

deepened :: Map h k v -> Map h k v -> Bool
-- | Find a difference in tilt between 2 versions of the branch.
deepened was @ Branch {} became @ Branch {}
    =   was ^.tilt == M
    &&  became^.tilt `elem` [L1, R1]
deepened Leaf{} Branch{} = True
deepened _      _        = False

shortened :: Map h k v -> Map h k v -> Bool
-- | Find a difference in tilt between 2 versions of the branch.
shortened was @ Branch {} became @ Branch {}
    =   was^.tilt `elem` [L1, R1]
    &&  became^.tilt == M
shortened Branch{} Leaf{} = True
shortened _        _      = False

roll :: Tilt -> Side -> Tilt
-- | Change tilt depending on grown side.
roll tilt side =
    case side of
      L -> pred tilt
      R -> succ tilt

change
    :: Hash h k v
    => (Zipped h k v a)
    -> Zipped h k v a
change action = do
    modus <- use mode
    when (modus == ReadonlyMode) $ do
        error "change: calling this in ReadonlyMode is prohibited"

    res <- action
    rev <- newRevision
    locus.revision .= rev
    return res

replaceWith :: Hash h k v => Map h k v -> Zipped h k v ()
replaceWith newTree = do
    change (locus .= newTree)

rebalance :: Hash h k v => Zipped h k v ()
-- | Perform rebalance (on one layer, used in insert).
rebalance = do
    tree <- use locus
    rev1 <- newRevision
    rev2 <- newRevision
    rev3 <- newRevision

    let node1 = branch rev1 M
    let node2 = branch rev2 M
    let node3 = branch rev3 M
    let skewn2 = branch rev2
    let skewn3 = branch rev3

    let
      newTree = case tree of
        Node L2 (Node L1 a b) c             -> node1 a (node2 b c)
        Node R2 a (Node R1 b c)             -> node1 (node2 a b) c

        Node L2 (Node R1 a (Node R1 b c)) d -> node1 (skewn2 L1 a b) (node3 c d)
        Node L2 (Node R1 a (Node L1 b c)) d -> node1 (node2 a b) (skewn3 R1 c d)
        Node L2 (Node R1 a (Node M  b c)) d -> node1 (node2 a b) (node3 c d)

        Node R2 a (Node L1 (Node R1 b c) d) -> node1 (skewn2 L1 a b) (node2 c d)
        Node R2 a (Node L1 (Node L1 b c) d) -> node1 (node2 a b) (skewn3 R1 c d)
        Node R2 a (Node L1 (Node M  b c) d) -> node1 (node2 a b) (node3 c d)

        Node R2 a (Node M b c)              -> skewn2 L1 (skewn3 R1 a b) c
        Node L2 (Node M a b) c              -> skewn2 R1 a (skewn3 L1 b c)

        other                               -> other

    replaceWith newTree

separately action = do
    state  <- get
    result <- action
    put state
    return result

track :: Show a => String -> a -> Zipped h k v ()
track msg val = do
    Debug.trace (msg <> " " <> show val) $ return ()

goto :: Hash h k v => k -> Zipped h k v ()
goto key = do
    raiseUntilHaveInRange key
    descentOnto key

raiseUntilHaveInRange :: Hash h k v => k -> Zipped h k v ()
raiseUntilHaveInRange key = goUp
  where
    goUp = do
        range <- use keyRange
        unless (key `inside` range) $ do
            up
            goUp
    inside k (l, h) = k >= l && k <= h

descentOnto :: Hash h k v => k -> Zipped h k v ()
descentOnto key = continueDescent
  where
    continueDescent = do
        center <- use (locus.centerKey)
        if key >= center
        then descentRight
        else descentLeft
        continueDescent
      <|> return ()
