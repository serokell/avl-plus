module Data.Tree.AVL.Set
  ( set
  ) where

import Control.Lens               (use, (.=))
import Control.Monad              (unless, void)

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Zipper

--import qualified Debug.Trace as Debug

-- | Insertion algorithm.
set :: forall h k v m . Retrieves h k v m => k -> Maybe v -> Zipped h k v m ()
set k v = do
    goto (Plain k)             -- teleport to a key (or near it if absent)
    withLocus $ \case
      MLEmpty {} -> do
        leaf0 <- leaf' k v minBound maxBound
        replaceWith leaf0
        return ()

      MLLeaf {_mlKey, _mlPrevKey, _mlNextKey} -> do
        let key0 = _mlKey
            prev = _mlPrevKey
            next = _mlNextKey

        if Plain k == key0 then do  -- update case, replace with new value
            change $ do
                here  <- use locus
                here' <- setValue v here
                locus .= here'
        else do
            if Plain k `isInside` (prev, key0)
            then do
                leaf0 <- leaf' k v prev key0

                splitInsertBefore leaf0

                unless (prev == minBound) $ do
                    goto prev
                    change $ do
                        here  <- use locus
                        here' <- setNextKey (Plain k) here
                        locus .= here'

            else do
                leaf0 <- leaf' k v key0 next

                splitInsertAfter leaf0

                unless (next == maxBound) $ do
                    goto next
                    change $ do
                        here  <- use locus
                        here' <- setPrevKey (Plain k) here
                        locus .= here'
      _ -> do
        error $ "insert: `goto k` ended in non-terminal node"

    return ()
  where
    splitInsertBefore :: Map h k v -> Zipped h k v m ()
    splitInsertBefore leaf0 = do
        tree <- use locus
        new  <- branch M leaf0 tree
        replaceWith new
        descentRight
        change $ do
            here  <- use locus
            here' <- setPrevKey (Plain k) here
            locus .= here'
        void up

    splitInsertAfter :: Map h k v -> Zipped h k v m ()
    splitInsertAfter leaf0 = do
        tree <- use locus
        new  <- branch M tree leaf0
        replaceWith new
        descentLeft
        change $ do
            here  <- use locus
            here' <- setNextKey (Plain k) here
            locus .= here'
        void up

    isInside k0 (l, h) = k0 >= l && k0 <= h
