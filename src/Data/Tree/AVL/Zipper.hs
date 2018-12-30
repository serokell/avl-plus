-- | This module represents zipper layer.
--   It allows us represent all operations using
--     'up', 'descent L', 'descent R' and 'change'
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
    , descent

      -- * Actions over iterator
    , withLocus
    , change
    , replaceWith
    , markHere

      -- * Current point iterator is standing on
    , locus
    , setLocus
    )
  where

import Control.Monad (unless, void)
import Control.Monad.Catch (catch, throwM)
import Control.Monad.State.Strict (StateT (runStateT), lift)

import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set (empty, fromList, insert)
import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof
import Data.Tree.AVL.Prune

import Lens.Micro.Platform (makeLenses, use, to, (%=), (.~))

import Control.Zipp as Zipp

type Zipped h k v m = Zipp.Action Side (Locus h k v) (WithTracking k h m)

type Range k = (WithBounds k, WithBounds k)

type WithTracking k h m = StateT (Context k h) m

data Context k h = Context
    { _cTouched  :: Set h
    , _cMode     :: Mode
    }

data Locus h k v = Locus
    { _lHere  :: Map h k v
    , _lRange :: Range k
    }

data Mode
    = UpdateMode
    | DeleteMode

makeLenses ''Context
makeLenses ''Locus

locus :: Retrieves h k v m => Zipped h k v m (Map h k v)
locus = peek lHere

setLocus :: Retrieves h k v m => Map h k v -> Zipped h k v m ()
setLocus tree = void $ change (lHere .~ tree)

-- | Run zipper operation, collect prefabricated proofs.
runZipped :: Retrieves h k v m => Zipped h k v m a -> Mode -> Map h k v -> m (a, Map h k v, Set h)
runZipped action mode tree = do
    ((a, (Locus tree1 _)), Context {_cTouched = set})
        <- with (Locus tree (Bottom, Top)) action
            `runStateT` Context
                { _cTouched = Set.empty
                , _cMode    = mode
                }
    
    return (a, tree1, set)

-- | Run zipper operation, build proof.
runZipped' :: Retrieves h k v m => Zipped h k v m a -> Mode -> Map h k v -> m (a, Map h k v, Proof h k v)
runZipped' action mode0 tree = do
    (a, tree1, trails) <- runZipped action mode0 tree
    proof              <- prune trails tree
    return (a, tree1, proof)

withLocus :: Retrieves h k v m => (MapLayer h k v (Map h k v) -> Zipped h k v m a) -> Zipped h k v m a
withLocus action = do
    loc   <- peek lHere
    layer <- lift $ lift $ load loc
    action layer

markHere :: (Monad m, Ord h) => Zipped h k v m ()
markHere = do
    h <- peek (lHere.to rootHash)
    lift $ cTouched %= Set.insert h

mark :: (Monad m, Ord h) => h -> WithTracking k h m ()
mark h = cTouched %= Set.insert h

markAll :: (Monad m, Ord h) => [h] -> WithTracking k h m ()
markAll hs = cTouched %= (<> Set.fromList hs)

descent :: forall h k v m . Retrieves h k v m => Side -> Zipped h k v m ()
descent = \case
    L -> go leftDir
    R -> go rightDir
  where
    leftDir :: Direction Side (Locus h k v) (WithTracking k h m)
    leftDir = Direction
        { designation = L
        , tearOut = \(Locus tree (low, _)) -> do
            mark (rootHash tree)
            result <- lift $ load tree >>= \case
                MLBranch { _mlLeft } ->
                    return _mlLeft
                _ ->
                    throwM CantGoThere
            
            mark (rootHash result)
            center <- lift $ centerKey result
            return (Locus result (low, center))

        , jamIn = \(Locus parentTree range) (Locus leftSubTree _) -> do
            lift (load parentTree) >>= \case
                MLBranch {_mlLeft, _mlRight, _mlTilt} -> do
                    mode     <- use cMode
                    tilt'    <- lift $ correctTilt mode _mlLeft leftSubTree _mlTilt L
                    result   <- lift $ branch tilt' leftSubTree _mlRight
                    balanced <- rebalance result
                    return (Locus balanced range)

                _ -> do
                    throwM CantGoUp
        }

    rightDir :: Direction Side (Locus h k v) (WithTracking k h m)
    rightDir = Direction
        { designation = R
        , tearOut = \(Locus tree (_, hi)) -> do
            mark (rootHash tree)
            result <- lift $ load tree >>= \case
                MLBranch { _mlRight } ->
                    return _mlRight
                _ ->
                    throwM CantGoThere
            
            mark (rootHash result)
            center <- lift $ centerKey result
            return (Locus result (center, hi))

        , jamIn = \(Locus parentTree range) (Locus rightSubTree _) -> do
            lift (load parentTree) >>= \case
                MLBranch {_mlLeft, _mlRight, _mlTilt} -> do
                    mode     <- use cMode
                    tilt'    <- lift $ correctTilt mode _mlRight rightSubTree _mlTilt R
                    result   <- lift $ branch tilt' _mlLeft rightSubTree
                    balanced <- rebalance result
                    return (Locus balanced range)

                _ -> do
                    throwM CantGoUp
        }

correctTilt :: Retrieves h k v m => Mode -> Map h k v -> Map h k v -> Tilt -> Side -> m Tilt
correctTilt mode was became tilt0 side = do
    deeper  <- deepened
    shorter <- shortened
    let
      -- If we inserted and tree became deeper,  increase tilt to that side.
      -- if we deleted  and tree became shorter, decrease tilt to that side.
      res = case mode of
        UpdateMode | deeper  -> roll side
        DeleteMode | shorter -> roll (another side)
        _                    -> tilt0

    return res
  where
    deepened = do
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

    shortened = do
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

    roll side' =
        case side' of
          L -> tiltLeft  tilt0
          R -> tiltRight tilt0

-- | Place new tree into the locus, UpdateMode its revision.
replaceWith :: Retrieves h k v m => Map h k v -> Zipped h k v m ()
replaceWith newTree = do
    void $ change (lHere .~ newTree)

-- | Fix imbalances around current locus of editation.
rebalance :: forall h k v m . Retrieves h k v m => Map h k v -> WithTracking k h m (Map h k v)
rebalance tree = do
    let hashes |- nodeGen = nodeGen <* markAll hashes

    let combine fork left right = lift $ do
            l <- left
            r <- right
            fork l r

    lift (load tree) >>= \case
      Node r1 L2 left d -> do
        lift (load left) >>= \case
          Node r2 L1 a b     -> [r1, r2]     |- combine (branch M)  (pure a)        (branch M  b d)
          Node r2 M  a b     -> [r1, r2]     |- combine (branch R1) (pure a)        (branch L1 b d)
          Node r2 R1 a right -> do
            lift (load right) >>= \case
              Node r3 R1 b c -> [r1, r2, r3] |- combine (branch M)  (branch L1 a b) (branch M  c d)
              Node r3 L1 b c -> [r1, r2, r3] |- combine (branch M)  (branch M  a b) (branch R1 c d)
              Node r3 M  b c -> [r1, r2, r3] |- combine (branch M)  (branch M  a b) (branch M  c d)
              _              -> return tree

          _ -> return tree

      Node r1 R2 a right -> do
        lift (load right) >>= \case
          Node r2 R1 b c     -> [r1, r2]     |- combine (branch M)  (branch M  a b) (pure c)
          Node r2 M  b c     -> [r1, r2]     |- combine (branch L1) (branch R1 a b) (pure c)
          Node r2 L1 left d  -> do
            lift (load left) >>= \case
              Node r3 R1 b c -> [r1, r2, r3] |- combine (branch M)  (branch L1 a b) (branch M  c d)
              Node r3 L1 b c -> [r1, r2, r3] |- combine (branch M)  (branch M  a b) (branch R1 c d)
              Node r3 M  b c -> [r1, r2, r3] |- combine (branch M)  (branch M  a b) (branch M  c d)
              _              -> return tree

          _ -> return tree

      _ -> return tree

-- | Teleport to previous key.
gotoPrevKey :: forall h k v m . Retrieves h k v m => k -> Zipped h k v m ()
gotoPrevKey k = do
    goto (Plain k)
    raiseUntilFrom R `catch` \CantGoUp    -> return ()
    descent L        `catch` \CantGoThere -> return ()
    whilePossible $ descent R

-- | Teleport to next key.
gotoNextKey :: forall h k v m . Retrieves h k v m => k -> Zipped h k v m ()
gotoNextKey k = do
    goto (Plain k)
    raiseUntilFrom L `catch` \CantGoUp    -> return ()
    descent R        `catch` \CantGoThere -> return ()
    whilePossible $ descent L

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
        range <- peek lRange
        unless (k `isInside` range) $ do
            _ <- up
            goUp

    k' `isInside` (l, h) = k' >= l && k' <= h

-- | Teleport to a 'Leaf' with given key from above.
descentOnto :: forall h k v m . Retrieves h k v m => WithBounds k -> Zipped h k v m ()
descentOnto key0 = continueDescent
  where
    continueDescent = do
        loc      <- peek lHere
        center   <- lift $ lift $ centerKey loc
        if key0 >= center
            then descent R
            else descent L
        continueDescent
      `catch` \CantGoThere -> do
        return ()
