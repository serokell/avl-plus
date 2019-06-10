
module Data.Tree.AVL.Adapter
    ( Proven
    , WrongOriginState (..)
    , DivergedWithProof (..)
    , insert
    , delete
    , lookup
    , require
    , record
    , record_
    , apply
    , rollback
    , transact
    , transactAndRethrow
    )
    where

import Prelude hiding (lookup)

import Control.Exception hiding (catch)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.Set as Set
import qualified Data.Tree.AVL as AVL
import Data.Typeable

import GHC.Generics (Generic)

-- | Insert key/value into the global tree.
insert
    :: AVL.Retrieves h k v m
    => k
    -> v
    -> SandboxT h k v m ()
insert k v = do
    tree <- get
    (set, tree') <- lift $ lift $ AVL.insert k v tree
    put tree'
    tell set

-- | Delete key from the global tree.
delete
    :: AVL.Retrieves h k v m
    => k
    -> SandboxT h k v m ()
delete k = do
    tree <- get
    (set, tree') <- lift $ lift $ AVL.delete k tree
    put tree'
    tell set

-- | Lookup key in the global tree.
lookup
    :: AVL.Retrieves h k v m
    => k
    -> SandboxT h k v m (Maybe v)
lookup k = do
    tree <- get
    ((res, set), tree') <- lift $ lift $ AVL.lookup k tree
    put tree'
    lift $ tell set
    return res

-- | Lookup key in the global tree.
require
    :: AVL.Retrieves h k v m
    => k
    -> SandboxT h k v m v
require k = do
    lookup k
        >>= maybe (throwM $ AVL.NotFound k) return

-- | Given a transaction and interpreter, perform it, write end tree into the storage and
--   equip the transaction with the proof and a hash of end state.
record
    :: AVL.Appends h k v m
    => tx
    -> (tx -> SandboxT h k v m a)
    -> m (a, Proven h k v tx)
record tx interp = do
    tree                <- AVL.currentRoot
    ((res, tree'), set) <- runWriterT $ runStateT (interp tx) tree
    proof               <- AVL.prune set tree

    AVL.append tree'

    return (res, Proven tx proof (AVL.rootHash tree'))

-- | Given a transaction and interpreter, perform it, write end tree into the storage and
--   equip the transaction with the proof and a hash of end state.
record_
    :: AVL.Appends h k v m
    => tx
    -> (tx -> SandboxT h k v m a)
    -> m (Proven h k v tx)
record_ tx interp = do
    tree                <- AVL.currentRoot
    ((_, tree'), set) <- runWriterT $ runStateT (interp tx) tree
    proof               <- AVL.prune set tree

    AVL.append tree'

    return (Proven tx proof (AVL.rootHash tree'))

data WrongOriginState h = WrongOriginState
    { wosExpected :: h
    , wosGot      :: h
    }
    deriving (Show)

instance (Show h, Typeable h) => Exception (WrongOriginState h)

data DivergedWithProof h = DivergedWithProof
    { dwpExpected :: h
    , dwpGot      :: h
    }
    deriving (Show)

instance (Show h, Typeable h) => Exception (DivergedWithProof h)

-- | The sandbox transformer to run `insert`, `delete` and `lookup` in.
type SandboxT h k v m = StateT (AVL.Map h k v) (WriterT (Set.Set h) m)

data Proven h k v tx = Proven
    { pTx      :: tx
    , pProof   :: AVL.Proof h k v
    , pEndHash :: h
    }
    deriving (Eq, Show, Generic)

-- | Using proven transaction, proof unwrapper and interpreter,
--   run the transaction.
apply
    :: AVL.Appends h k v m
    => Proven h k v tx
    -> (tx -> SandboxT h k v m a)
    -> m a
apply (Proven tx proof endHash) interp = do
    tree <- AVL.currentRoot

    unless (AVL.rootHash tree `AVL.checkProof` proof) $ do
        throwM WrongOriginState
            { wosExpected = AVL.rootHash (AVL.unProof proof)
            , wosGot      = AVL.rootHash  tree
            }

    let tree' = AVL.unProof proof
    AVL.append tree'

    ((res, tree''), _) <- runWriterT $ runStateT (interp tx) tree'

    unless (AVL.rootHash tree'' == endHash) $ do
        throw DivergedWithProof
            { dwpExpected = endHash
            , dwpGot      = AVL.rootHash tree''
            }

    AVL.append tree''

    return res

-- | Using proven transaction, proof unwrapper and interpreter,
--   run the transaction.
rollback
    :: AVL.Appends h k v m
    => Proven h k v tx
    -> (tx -> SandboxT h k v m a)
    -> m a
rollback (Proven tx proof endHash) interp = do
    tree <- AVL.currentRoot

    unless (AVL.rootHash tree == endHash) $ do
        throw WrongOriginState
            { wosExpected = endHash
            , wosGot      = AVL.rootHash tree
            }

    let tree' = AVL.unProof proof
    AVL.append tree'

    ((res, tree''), _) <- runWriterT $ runStateT (interp tx) tree'

    unless (AVL.rootHash tree'' == AVL.rootHash tree) $ do
        throw DivergedWithProof
            { dwpExpected = AVL.rootHash tree
            , dwpGot      = AVL.rootHash tree''
            }

    AVL.append tree'
    return res

-- | If something fails, restore to pristine state.
transact
    :: forall e h k v m a
    .  (Exception e, AVL.Appends h k v m)
    => m a
    -> m (Either e a)
transact action = do
    saved <- AVL.currentRoot
    (Right <$> action) `catch` \e -> do
        AVL.append saved
        return (Left e)

-- | If something fails, restore to pristine state and rethrow.
transactAndRethrow
    :: forall e h k v m a
    .  (Exception e, AVL.Appends h k v m)
    => m a
    -> m a
transactAndRethrow action = do
    transact @e action >>= either throwM return

-- | The client proof unwrapping: retrieves the pruned tree from proof.
unpackOnClient :: Monad m => AVL.Proof h k v -> m (AVL.Map h k v)
unpackOnClient (AVL.Proof p) = return p

-- | The server proof unwrapping: uses current tree instead.
unpackOnServer :: AVL.Appends h k v m => AVL.Proof h k v -> m (AVL.Map h k v)
unpackOnServer _ = AVL.currentRoot
