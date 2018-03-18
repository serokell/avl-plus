
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Tree.AVL.Insertion
  ( insert
  , insertWithNoProof
  , fromList
  , fromFoldable
  , insert'
  ) where

import Control.Lens (use, (%=))
import Control.Monad (unless, foldM)
import Control.Monad.Trans.Class (lift)

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof
import Data.Tree.AVL.Zipper

import qualified Debug.Trace as Debug

-- | Endpoint that allows to merge proofs for some sequental operations.
insert' :: Stores h k v m => k -> v -> Map h k v m -> m (RevSet, Map h k v m)
insert' k v tree = do
    ((), res, trails) <- runZipped' (insertZ k v) UpdateMode tree
    return (trails, res)

-- | Endpoint that generates proof.
insert :: Stores h k v m => k -> v -> Map h k v m -> m (Proof h k v, Map h k v m)
insert k v tree = do
    ((), res, proof) <- runZipped (insertZ k v) UpdateMode tree
    return (proof, res)

-- | Endpoint that generates no proof.
insertWithNoProof
    :: Stores h k v m
    => k
    -> v
    -> Map h k v m
    -> m (Map h k v m)
insertWithNoProof k v tree = do
    ((), res, _) <- runZipped (insertZ k v) UpdateMode tree
    return res

-- | Insertion algorithm.
insertZ :: forall h k v m . Stores h k v m => k -> v -> Zipped h k v m ()
insertZ k v = do
    () <- Debug.trace "goto k" $ return ()
    goto k             -- teleport to a key (or near it if absent)
    () <- Debug.trace "use locus" $ return ()
    tree <- use locus
    layer <- lift $ pick tree
    case layer of
      MLEmpty {} -> do
        leaf0 <- createLeaf k v minBound maxBound
        replaceWith leaf0

      MLLeaf {_mlKey, _mlPrevKey, _mlNextKey} -> do
        let key0 = _mlKey
            prev = _mlPrevKey
            next = _mlNextKey

        if k == key0 then do  -- update case, replace with new value
            change $ do
                locus %= setValue v
        else do
            if k `isInside` (prev, key0)
            then do
                leaf0 <- createLeaf k v prev key0

                splitInsertBefore leaf0
                unless (prev == minBound) $ do
                    goto prev
                    change $ do
                        locus %= setNextKey k

            else do
                leaf0 <- createLeaf k v key0 next

                splitInsertAfter leaf0
                unless (next == maxBound) $ do
                    goto next
                    change $ do
                        locus %= setPrevKey k
      _ -> do
        error $ "insert: `goto k` ended in non-terminal node"

    return ()
  where
    splitInsertBefore :: Map h k v m -> Zipped h k v m ()
    splitInsertBefore leaf0 = do
        tree <- use locus
        rev  <- newRevision
        replaceWith (branch rev M leaf0 tree)
        descentRight
        change $ do
            locus %= setPrevKey k

    splitInsertAfter :: Map h k v m -> Zipped h k v m ()
    splitInsertAfter leaf0 = do
        tree <- use locus
        rev  <- newRevision
        replaceWith (branch rev M tree leaf0)
        descentLeft
        change $ do
            locus %= setNextKey k

    createLeaf :: k -> v -> k -> k -> Zipped h k v m (Map h k v m)
    createLeaf k0 v0 prev next = do
        rev <- newRevision
        return $ leaf rev k0 v0 prev next

    isInside k0 (l, h) = k0 >= l && k0 <= h

fromList :: Stores h k v m
    => [(k, v)]
    -> m (Map h k v m)
-- | Monomorphised version.
fromList = fromFoldable

fromFoldable :: Stores h k v m => Foldable f => f (k, v) -> m (Map h k v m)
-- | Construct a tree from any Foldable (and calculate all hashes).
fromFoldable = foldM (flip $ uncurry insertWithNoProof) empty
