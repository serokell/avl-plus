
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Data.Tree.AVL.Insertion
  ( insert
  , insertWithNoProof
  , fromList
  , fromFoldable
  , insert'
  ) where

import Control.Lens (use, (%=), (.=))
import Control.Monad (unless, foldM, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof
import Data.Tree.AVL.Zipper

import qualified Debug.Trace as Debug

-- | Endpoint that allows to merge proofs for some sequental operations.
insert' :: Stores h k v m => k -> v -> Map h k v -> m (RevSet, Map h k v)
insert' k v tree = do
    ((), res, trails) <- runZipped' (insertZ k v) UpdateMode tree
    return (trails, res)

-- | Endpoint that generates proof.
insert :: Stores h k v m => k -> v -> Map h k v -> m (Proof h k v, Map h k v)
insert k v tree = do
    ((), res, proof) <- runZipped (insertZ k v) UpdateMode tree
    return (proof, res)

-- | Endpoint that generates no proof.
insertWithNoProof
    :: Stores h k v m
    => k
    -> v
    -> Map h k v
    -> m (Map h k v)
insertWithNoProof k v tree = do
    ((), res, _) <- runZipped (insertZ k v) UpdateMode tree
    return res

-- | Insertion algorithm.
insertZ :: forall h k v m . Stores h k v m => k -> v -> Zipped h k v m ()
insertZ k v = do
    dump "insert"
    goto k             -- teleport to a key (or near it if absent)
    tree <- use locus
    lift (open tree) >>= \case
      MLEmpty {} -> do
        leaf0 <- createLeaf k v minBound maxBound
        replaceWith leaf0
        return ()

      MLLeaf {_mlKey, _mlPrevKey, _mlNextKey} -> do
        let key0 = _mlKey
            prev = _mlPrevKey
            next = _mlNextKey

        if k == key0 then do  -- update case, replace with new value
            change $ do
                here  <- use locus
                here' <- lift $ setValue v here
                locus .= here'
        else do
            if k `isInside` (prev, key0)
            then do
                leaf0 <- createLeaf k v prev key0
                
                splitInsertBefore leaf0
                
                unless (prev == minBound) $ do
                    goto prev
                    change $ do
                        here  <- use locus
                        here' <- lift $ setNextKey k here
                        locus .= here'

            else do
                leaf0 <- createLeaf k v key0 next

                splitInsertAfter leaf0
                
                unless (next == maxBound) $ do
                    goto next
                    change $ do
                        here  <- use locus
                        here' <- lift $ setPrevKey k here
                        locus .= here'
      _ -> do
        error $ "insert: `goto k` ended in non-terminal node"

    return ()
  where
    splitInsertBefore :: Map h k v -> Zipped h k v m ()
    splitInsertBefore leaf0 = do
        tree <- use locus
        rev  <- newRevision
        new  <- lift $ branch rev M leaf0 tree
        replaceWith new
        descentRight
        change $ do
            here  <- use locus
            here' <- lift $ setPrevKey k here
            locus .= here'
        void up

    splitInsertAfter :: Map h k v -> Zipped h k v m ()
    splitInsertAfter leaf0 = do
        tree <- use locus
        rev  <- newRevision
        new  <- lift $ branch rev M tree leaf0
        replaceWith new
        descentLeft
        change $ do
            here  <- use locus
            here' <- lift $ setNextKey k here
            locus .= here'
        void up

    createLeaf :: k -> v -> k -> k -> Zipped h k v m (Map h k v)
    createLeaf k0 v0 prev next = do
        rev <- newRevision
        lift $ leaf rev k0 v0 prev next

    isInside k0 (l, h) = k0 >= l && k0 <= h

fromList :: Stores h k v m
    => [(k, v)]
    -> m (Map h k v)
-- | Monomorphised version.
fromList = fromFoldable

fromFoldable :: forall h k v m f . Stores h k v m => Foldable f => f (k, v) -> m (Map h k v)
-- | Construct a tree from any Foldable (and calculate all hashes).
fromFoldable list = do
    e <- empty
    foldM push e list
  where
    push :: Map h k v -> (k, v) -> m (Map h k v)
    push tree (k, v) = insertWithNoProof k v tree
