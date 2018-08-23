module Data.Tree.AVL.Insertion
  ( insert
  , insertWithNoProof
  , fromList
  , fromFoldable
  , insert'
  ) where

import Control.Monad              (foldM)

import Data.Set                   (Set)

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof
import Data.Tree.AVL.Zipper
import Data.Tree.AVL.Set

--import qualified Debug.Trace as Debug

-- | Endpoint that allows to merge proofs for some sequental operations.
insert' :: Retrieves h k v m => k -> v -> Map h k v -> m (Set h, Map h k v)
insert' k v tree = do
    ((), res, trails) <- runZipped' (insertZ k v) UpdateMode tree
    return (trails, res)

-- | Endpoint that generates proof.
insert :: Retrieves h k v m => k -> v -> Map h k v -> m (Proof h k v, Map h k v)
insert k v tree = do
    ((), res, proof) <- runZipped (insertZ k v) UpdateMode tree
    return (proof, res)

-- | Endpoint that generates no proof.
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
insertZ k v = set k (Just v)

fromList :: Retrieves h k v m
    => [(k, v)]
    -> m (Map h k v)
-- | Monomorphised version.
fromList = fromFoldable

fromFoldable :: forall h k v m f . Retrieves h k v m => Foldable f => f (k, v) -> m (Map h k v)
-- | Construct a tree from any Foldable (and calculate all hashes).
fromFoldable list = do
    foldM push empty list
  where
    push :: Map h k v -> (k, v) -> m (Map h k v)
    push tree (k, v) = insertWithNoProof k v tree
