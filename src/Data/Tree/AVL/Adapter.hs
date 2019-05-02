
module Data.Tree.AVL.Adapter where

import Prelude hiding (lookup)

import Control.Exception hiding (catch)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.Set as Set
import qualified Data.Tree.AVL as AVL

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
proven
    :: AVL.Appends h k v m
    => tx
    -> (tx -> SandboxT h k v m a)
    -> m (a, Proven h k v tx)
proven tx interp = do
    tree                <- AVL.currentRoot
    ((res, tree'), set) <- runWriterT $ runStateT (interp tx) tree
    proof               <- AVL.prune set tree

    AVL.append tree'

    return (res, Proven tx proof (AVL.rootHash tree'))

-- | Thrown when the proof mismatches current tree.
data BeginHashMismatch = BeginHashMismatch
    deriving (Show)

instance Exception BeginHashMismatch

-- | Thrown when the proposed endhash and real endhash diverge.
data EndHashMismatch = EndHashMismatch
    deriving (Show)

instance Exception EndHashMismatch

-- | The sandbox transformer to run `insert`, `delete` and `lookup` in.
type SandboxT h k v m = StateT (AVL.Map h k v) (WriterT (Set.Set h) m)

-- | The client proof unwrapping: retrieves the pruned tree from proof.
unpackClient :: Monad m => AVL.Proof h k v -> m (AVL.Map h k v)
unpackClient (AVL.Proof p) = return p

-- | The server proof unwrapping: uses current tree instead.
unpackServer :: AVL.Appends h k v m => AVL.Proof h k v -> m (AVL.Map h k v)
unpackServer _ = AVL.currentRoot

data Proven h k v tx = Proven
    { pTx      :: tx
    , pProof   :: AVL.Proof h k v
    , pEndHash :: h
    }

-- | Using proven transaction, proof unwrapper and interpreter,
--   run the transaction.
prove
    :: AVL.Appends h k v m
    => Proven h k v tx
    -> (AVL.Proof h k v -> m (AVL.Map h k v))
    -> (tx -> SandboxT h k v m a)
    -> m a
prove (Proven tx proof endHash) unpack interp = do
    tree <- AVL.currentRoot

    unless (AVL.rootHash tree `AVL.checkProof` proof) $ do
        throw BeginHashMismatch

    tree'              <- unpack proof
    ((res, tree''), _) <- runWriterT $ runStateT (interp tx) tree'

    unless (AVL.rootHash tree'' == endHash) $ do
        throw EndHashMismatch

    AVL.append tree''

    return res

-- | Using proven transaction, proof unwrapper and interpreter,
--   run the transaction.
rollback
    :: AVL.Appends h k v m
    => Proven h k v tx
    -> (AVL.Proof h k v -> m (AVL.Map h k v))
    -> (tx -> SandboxT h k v m a)
    -> m a
rollback (Proven tx proof endHash) unpack interp = do
    tree <- AVL.currentRoot

    unless (AVL.rootHash tree == endHash) $ do
        throw EndHashMismatch

    tree' <- unpack proof

    ((res, tree''), _) <- runWriterT $ runStateT (interp tx) tree'

    unless (AVL.rootHash tree'' == endHash) $ do
        throw EndHashMismatch

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
