-- | Key removal.
--
--   Can be repeated on the proof it generates with the same result.

module Data.Tree.AVL.Deletion
    ( delete
    , deleteWithNoProof
    , delete'
    ) where

import Control.Lens (use, (.=))
import Control.Monad (unless)
import Data.Set (Set)

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof
import Data.Tree.AVL.Zipper

-- | Remove given key from the 'Map', generates raw proof.
delete :: Retrieves h k v m => k -> Map h k v -> m (Set Revision, Map h k v)
delete k tree = do
    (_yes, res, trails) <- runZipped (deleteZ k) DeleteMode tree
    return (trails, res)

-- | Remove given key from the 'Map', generates baked proof.
--
--   It is idempotent.
delete' :: Retrieves h k v m => k -> Map h k v -> m (Proof h k v, Map h k v)
delete' k tree = do
    (_yes, res, proof) <- runZipped' (deleteZ k) DeleteMode tree
    return (proof, res)

-- | Remove given key from the 'Map', with no proof.
deleteWithNoProof
    :: Retrieves h k v m
    => k
    -> Map h k v
    -> m (Map h k v)
deleteWithNoProof k tree = do
    (_yes, res, _proof) <- runZipped (deleteZ k) DeleteMode tree
    return res

-- | Deletion algorithm.
deleteZ :: forall h k v m . Retrieves h k v m => k -> Zipped h k v m Bool
deleteZ k = withLocus $ \case
    MLLeaf { _mlKey } ->
        if _mlKey == Plain k
        then True  <$ replaceWith (empty :: Map h k v)
        else False <$ mark "deleteZ/node exists"

    MLEmpty {} -> False <$ mark "deleteZ/empty"

    MLBranch {} -> do
        goto (Plain k)
        withLocus $ \case
          MLLeaf { _mlKey = key0, _mlPrevKey = prev, _mlNextKey = next } -> do
            if key0 /= Plain k
            then return False
            else do
                side <- up  -- return to a parent of node to be deleted

                -- we need to mark another child, so it ends in a proof
                _ <- case side of
                    L -> descentRight >> up
                    R -> descentLeft  >> up

                newTree <- withLocus $ \case
                    MLBranch { _mlLeft = left, _mlRight = right } ->
                        return $ case side of
                            L -> right
                            R -> left
                    _ -> error "delete: successful `up` ended in non-Branch"

                replaceWith newTree  -- replace with another child

                unless (prev == minBound) $ do
                    goto prev
                    change $ do
                        loc   <- use locus
                        loc'  <- setNextKey next loc
                        locus .= loc'

                unless (next == maxBound) $ do
                    goto next
                    change $ do
                        loc   <- use locus
                        loc'  <- setPrevKey prev loc
                        locus .= loc'

                return True

          _ -> error $ "insert: `goto ended in non-terminal node"
