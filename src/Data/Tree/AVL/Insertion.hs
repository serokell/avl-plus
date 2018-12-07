-- | New/update operation.
--
--   Can be repeated on the proof it generates with the same result.

module Data.Tree.AVL.Insertion
    ( -- Different variants of insert
      insert
    , insert'
    , insertWithNoProof

      -- 'Map' constructors
    , fromList
    , fromFoldable
    ) where

import Control.Lens (use, (.=))
import Control.Monad (foldM, unless, void)
import Data.Set (Set)

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof
import Data.Tree.AVL.Zipper


-- | Inserts given value for given key into the 'Map', generates
--   raw proof.
--
--   It is idempotent in terms of 'Map' content, however, without 'Eq' @k@
--   constraint 'Map's will be different.
insert :: Retrieves h k v m => k -> v -> Map h k v -> m (Set h, Map h k v)
insert k v tree = do
    ((), res, trails) <- runZipped (insertZ k v) UpdateMode tree
    return (trails, res)

-- | Inserts given value for given key into the 'Map', generates baked proof.
--
--   See 'insert''.
insert' :: Retrieves h k v m => k -> v -> Map h k v -> m (Proof h k v, Map h k v)
insert' k v tree = do
    ((), res, proof) <- runZipped' (insertZ k v) UpdateMode tree
    return (proof, res)

-- | Just inserts given value for given key into the 'Map', with no proof.
--
--   See 'insert''.
insertWithNoProof
    :: Retrieves h k v m
    => k
    -> v
    -> Map h k v
    -> m (Map h k v)
insertWithNoProof k v tree = do
    ((), res, _) <- runZipped (insertZ k v) UpdateMode tree
    return res

-- | Insertion algorithm.
insertZ :: forall h k v m . Retrieves h k v m => k -> v -> Zipped h k v m ()
insertZ k v = do
    goto (Plain k)             -- teleport to a key (or near it if absent)
    withLocus $ \case
      MLEmpty {} -> do
        replaceWith =<< leaf k v minBound maxBound
        return ()

      MLLeaf {_mlKey, _mlPrevKey, _mlNextKey} -> do
        let key0 = _mlKey
            prev = _mlPrevKey
            next = _mlNextKey
        if Plain k == key0  -- update case, replace with new value
        then
            change $ do
                here  <- use locus
                here' <- setValue v here
                locus .= here'
        else do
            if Plain k `isInside` (prev, key0)
            then do
                leaf0 <- leaf k v prev key0

                splitInsertBefore leaf0

                unless (prev == minBound) $ do
                    goto prev
                    change $ do
                        here  <- use locus
                        here' <- setNextKey (Plain k) here
                        locus .= here'

            else do
                leaf0 <- leaf k v key0 next

                splitInsertAfter leaf0

                unless (next == maxBound) $ do
                    goto next
                    change $ do
                        here  <- use locus
                        here' <- setPrevKey (Plain k) here
                        locus .= here'
      _ -> error $ "insert: `goto k` ended in non-terminal node"
  where
    splitInsertBefore :: Map h k v -> Zipped h k v m ()
    splitInsertBefore leaf0 = do
        tree <- use locus
        replaceWith =<< branch M leaf0 tree
        descentRight
        change $ do
            here  <- use locus
            here' <- setPrevKey (Plain k) here
            locus .= here'
        void up

    splitInsertAfter :: Map h k v -> Zipped h k v m ()
    splitInsertAfter leaf0 = do
        tree <- use locus
        replaceWith =<< branch M tree leaf0
        descentLeft
        change $ do
            here  <- use locus
            here' <- setNextKey (Plain k) here
            locus .= here'
        void up

    isInside k0 (l, h) = k0 >= l && k0 <= h

-- | Monomorphised version of 'fromFoldable'.
fromList :: Retrieves h k v m => [(k, v)] -> m (Map h k v)
fromList = fromFoldable

fromFoldable ::
       forall h k v m f. (Retrieves h k v m, Foldable f)
    => f (k, v)
    -> m (Map h k v)
-- | Construct a tree from any Foldable (and calculate all hashes).
fromFoldable list = fullRehash <$> foldM push empty list
  where
    push :: Map h k v -> (k, v) -> m (Map h k v)
    push tree (k, v) = insertWithNoProof k v tree
