{-|
    AVL+ tree.

    To use it, you need to implement 'KVRetrieve', 'KVStore' and optionally,
    'KVMutate' typeclasses for a monad you want to run actions in.

    Do not use methods from KVWhatever-instances yourself! Use methods exported from this module.

    The tree consists of 2 parts: materialised and non-materialised (= in-storage part).

    Materialised tree is actual persistent tree structure.

    Non-materialised tree is a kv-storage, filled with (hash -> node) relation,
    which may or may not be persistent.

    Typical tree workflow is as follows:

    (1) Get the tree.

        You either get it `Data.Tree.AVL.fromList` or using 'Data.Tree.AVL.currentRoot'.
        Former will create materialised tree from list of kv-pairs and latter
        will give you non-materialised tree which was an argument of last `append` or `overwrite`
        call (or just 'empty' tree, if storage was clean).

    (2) Do things with it.

        In fact, it behaves like ordinal `Data.Map.Map`, but with minimalistic interface.
        You can 'Data.Tree.AVL.insert', 'Data.Tree.AVL.delete' or 'Data.Tree.AVL.lookup' on it,
        whether it is materialised or not.

        Each of these operations return new tree and proof prefab in form of 'Set' 'Revision'
        you have to store somewhere. It is suggested you use each new tree produced as linearily
        as possible and collect all the proof, like that:

        > do  start                  <- AVL.currentRoot
        >     (     nodeset1,  tree) <- AVL.insert "foo" 1 start
        >     ((mv, nodeset2), tree) <- AVL.lookup "bar"   tree
        >     (     nodeset3,  tree) <- AVL.delete "qux"   tree
        >     ()                     <- AVL.overwrite tree
        >     proof                  <- AVL.prune (nodeset1 <> nodeset2 <> nodeset3) tree
        >     return (mv, proof)

        This block will return pair of @(Maybe v, AVL.Proof h k v)@. Latter allows you to unpack
        materialised tree out of it an replay the same operations even on separate machine
        to produce the same result. And this tree will be a lot smaller than full tree.

        It is not nessessary to use tree produced by 'Data.Tree.AVL.lookup';
        however, path to node you've searched for will be materialised, which may speed
        things up.

    (3) Store tree or throw it out.

        If you call `append` or `overwrite` the tree you pass in will become new root in storage.
        The `overwrite` will erase previous version from the storage, but
        `append` will not, allowing you to rollback for O(1).

        If you throw tree out, storage will not change.

        These two operations are essentially transaction writers, with `overwrite` requiring
        write-lock on storage, since it removes nodes from it.

        The `append` operation only adds nodes to storage, so no locking is needed, you can read
        while you write. The only operation to notice changes in storage is `currentRoot`.

    There are default storages, like 'Data.Tree.AVL.Store.Void' and 'Data.Tree.AVL.Store.Pure'.

    'Data.Tree.AVL.Store.Void' is an 'IO' monad and 'Data.Tree.AVL.Store.Void.VoidStorageT'
    transformer. Former and latter both throw 'NotFound' on any re
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
    , Hash(hashOf)
    , Params
    , Base

      -- * Exceptions
    , NotFound (..)

      -- * Main type ('Map') and all other types
    , Map
    , MapLayer (..)
    , Tilt
    , WithBounds
    , Revision

      -- * Constructors/toList
    , empty
    , fromList
    , fromFoldable
    , toList

      -- * Root hash signature
    , rootHash
    , emptyHash

      -- * Operations over AVL+
    , insert
    , delete
    , deleteWithNoProof
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
    , fullRehash
    , prune
    , checkProof

      -- * For debug
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
