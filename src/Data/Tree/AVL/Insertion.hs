-- | New/update operation.
--
--   Can be repeated on the proof it generates with the same result.

module Data.Tree.AVL.Insertion
    ( -- Different variants of insert
      insert
    , insertWithNoProof

      -- 'Map' constructors
    , fromList
    , fromFoldable
    ) where

import Control.Monad (foldM, void)
import Control.Monad.Reader (lift)
import Data.Set (Set)

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Zipper


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
    (_, res) <- insert k v tree
    return res

-- | Inserts given value for given key into the 'Map', generates
--   raw proof.
--
--   It is idempotent in terms of 'Map' content, however, without 'Eq' @k@
--   constraint 'Map's will be different.
insert :: forall h k v m. Retrieves h k v m => k -> v -> Map h k v -> m (Set h, Map h k v)
insert k v tree = execZipped UpdateMode tree $ do
    goto (Plain k)  -- teleport to a key (or near it if absent)
    withLocus $ \case
      MLEmpty {} -> do
        replaceWith =<< lift (lift $ leaf k v)

      MLLeaf { _mlKey = key0 } -> do
        if k == key0  -- update case, replace with new value
        then do
            here  <- locus
            here' <- lift $ lift $ setValue v here
            setLocus here'
        else do
            if k < key0
            then do
                splitInsertBefore =<< lift (lift $ leaf k v)
                gotoPrevKey k

            else do
                splitInsertAfter =<< lift (lift $ leaf k v)
                gotoNextKey k

      _ -> error $ "insert: `goto k` ended in non-terminal node"
  where
    splitInsertBefore :: Map h k v -> Zipped h k v m ()
    splitInsertBefore leaf0 = do
        here <- locus
        replaceWith =<< lift (lift $ branch M leaf0 here)
        descent R
        void up

    splitInsertAfter :: Map h k v -> Zipped h k v m ()
    splitInsertAfter leaf0 = do
        here <- locus
        replaceWith =<< lift (lift $ branch M here leaf0)
        descent L
        void up

-- | Monomorphised version of 'fromFoldable'.
fromList :: Retrieves h k v m => [(k, v)] -> m (Map h k v)
fromList = fromFoldable

fromFoldable ::
       forall h k v m f. (Retrieves h k v m, Foldable f)
    => f (k, v)
    -> m (Map h k v)
-- | Construct a tree from any Foldable (and calculate all hashes).
fromFoldable = foldM push empty
  where
    push :: Map h k v -> (k, v) -> m (Map h k v)
    push tree (k, v) = insertWithNoProof k v tree
