{-|
    AVL+ tree.

    To use it, you need to implement 'KVRetrieve', 'KVStore' and,
    optionally, 'KVMutate' typeclasses for a monad you want to run
    actions in.

    Do not use methods from KVWhatever-instances yourself! Use methods
    exported from this module.

    The tree consists of 2 parts: materialised and non-materialised
    (i.e. in-storage part). Materialised tree is an actual persistent
    tree structure. Non-materialised tree is a kv-storage, filled with
    (hash -> node) relation, which may or may not be persistent.

    Typical workflow is as follows:

    (1) Get the tree.

        You either get it usng `Data.Tree.AVL.fromList` or
        'Data.Tree.AVL.currentRoot'.  The former will create
        materialised tree from a list of kv-pairs, and the latter on
        will give you a non-materialised tree which was an argument of
        last `append` or `overwrite` call (or just 'empty' tree, if
        storage was clean).

    (2) Perform actions on it.

        The tree behaves like an ordinal `Data.Map.Map`, but with
        a minimalistic interface. You can 'Data.Tree.AVL.insert',
        'Data.Tree.AVL.delete' or 'Data.Tree.AVL.lookup' on it,
        (both on materialised version and not).

        Each of these operations returns a new tree and a proof in
        form of a 'Set' 'Revision' you have to store somewhere. It is
        suggested that you use each new tree produced as linearly as
        possible and collect all the proofs, like this:

        > do  start                  <- AVL.currentRoot
        >     (     nodeset1,  tree) <- AVL.insert "foo" 1 start
        >     ((mv, nodeset2), tree) <- AVL.lookup "bar"   tree
        >     (     nodeset3,  tree) <- AVL.delete "qux"   tree
        >     proof                  <- AVL.prune (nodeset1 <> nodeset2 <> nodeset3) start
        >     ()                     <- AVL.overwrite tree
        >     return (mv, proof)

        This block will return a pair @(Maybe v, AVL.Proof h k v)@.
        The second element allows you to unpack the materialised tree
        and replay the same operations even on a separate
        machine to produce the same result. And this tree will be a
        lot smaller than a full one.

        It is not nessessary to use tree produced by
        'Data.Tree.AVL.lookup'; however, the path to node you've
        searched for will be materialised, which may speed things up.

    (3) Store the tree or throw it away.

        If you call `append` or `overwrite` the tree you pass will
        become a new root in the storage.  The `overwrite` will erase
        a previous version from the storage, but `append` will not,
        allowing you to rollback for O(1).

        If you throw tree out, storage will not change.

        These two operations are essentially transaction writers, with
        `overwrite` requiring write-lock on storage, since it removes
        nodes from it.

        The `append` operation only adds nodes to the storage, so no
        locking is needed, you can read while you write. The only
        operation to notice changes in storage is `currentRoot`.

    There are several storages -- see 'Data.Tree.AVL.Store.Pure' (pure
    storage) and 'Data.Tree.AVL.Store.Void' (for testing purposes).

-}
module Data.Tree.AVL
    ( -- * Required interfaces
      KVRetrieve (..)
    , KVStore (..)
    , KVMutate (..)

      -- * Constraints for most AVL+-related actions
    , Retrieves
    , Stores
    , Mutates

      -- * Some base constraints
    , Hash
    , Base
    , Params
    , ProvidesHash (..)

      -- * Exceptions
    , NotFound (..)

      -- * Main type ('Map') and all other types
    , Map
    , MapLayer
    , MapLayerTemplate (..)

      -- * Constructors/toList
    , empty
    , ref
    , fromList
    , fromFoldable
    , toList

      -- * Root hash signature
    , rootHash
    , emptyHash

      -- * Operations over AVL+
    , insert, insertWithNoProof
    , delete, deleteWithNoProof
    , lookup
    , fold
    , foldIf

      -- * Operations that change storage or request current state
    , save
    , append
    , overwrite
    , currentRoot
    , NoRootExists(..)
    , initialiseStorageIfNotAlready

      -- * Proof type, ways to construct and eliminate
    , Proof (..)
    , prune
    , checkProof

      -- * Serialisation helpers
    , beforeSerialise
    , beforeSerialiseLayer
    , afterDeserialise
    , afterDeserialiseLayer

      -- * Helpers
    , showMap
    ) where

import Prelude hiding (lookup)

import Data.Tree.AVL.Deletion
import Data.Tree.AVL.Insertion
import Data.Tree.AVL.Internal
import Data.Tree.AVL.Iteration
import Data.Tree.AVL.Lookup
import Data.Tree.AVL.Proof
import Data.Tree.AVL.Prune
import Data.Tree.AVL.Unsafe
