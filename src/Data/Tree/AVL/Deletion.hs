
module Data.Tree.AVL.Deletion (delete, deleteWithNoProof, delete') where

import Data.Set                   (Set)

import Data.Tree.AVL.Internal
import Data.Tree.AVL.Proof
import Data.Tree.AVL.Zipper
import Data.Tree.AVL.Set

-- | Endpoint that allows to merge proofs for some sequental operations.
delete' :: Retrieves h k v m => k -> Map h k v -> m (Set h, Map h k v)
delete' k tree = do
    (_yes, res, trails) <- runZipped' (deleteZ k) DeleteMode tree
    return (trails, res)

-- | Endpoint that generates proof.
delete :: Retrieves h k v m => k -> Map h k v -> m (Proof h k v, Map h k v)
delete k tree = do
    (_yes, res, proof) <- runZipped (deleteZ k) DeleteMode tree
    return (proof, res)

-- | Endpoint that generates no proof.
deleteWithNoProof
    :: Retrieves h k v m
    => k
    -> Map h k v
    -> m (Map h k v)
deleteWithNoProof k tree = do
    (_yes, res, _proof) <- runZipped (deleteZ k) DeleteMode tree
    return res

-- | Deletion algorithm.
deleteZ :: forall h k v m . Retrieves h k v m => k -> Zipped h k v m ()
deleteZ k = set k Nothing
