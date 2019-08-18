
module Data.Blockchain.Storage.AVL
    ( Proven
    , SandboxT
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
    , pair
    , LightNode
    , runOnLightNode
    , AVL.genesis
    )
    where

import Prelude hiding (lookup)

import Control.Exception hiding (catch)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Lens ((#), (^?))

import qualified Data.Set as Set
import qualified Data.Tree.AVL as AVL
import Data.Typeable
import Data.Union
import Data.Relation

import GHC.Generics (Generic)

inSandbox :: (Ord h, Monad m) => m a -> SandboxT h k v m a
inSandbox = lift . lift

-- | Insert key/value into the global tree.
insert
    :: forall k v h ks vs m
    .  AVL.Retrieves h ks vs m
    => Member k ks
    => Member v vs
    => Relates k v
    => k
    -> v
    -> SandboxT h ks vs m ()
insert k v = do
    tree <- get
    (set, tree') <- inSandbox $ AVL.insert (union # k) (union # v) tree
    put tree'
    tell set

-- | Delete key from the global tree.
delete
    :: forall k h ks vs m
    .  AVL.Retrieves h ks vs m
    => Member k ks
    => k
    -> SandboxT h ks vs m ()
delete k = do
    tree <- get
    (set, tree') <- inSandbox $ AVL.delete (union # k) tree
    put tree'
    tell set

-- | Lookup key in the global tree.
lookup
    :: forall k v h ks vs m
    .  AVL.Retrieves h ks vs m
    => Member k ks
    => Member v vs
    => Relates k v
    => k
    -> SandboxT h ks vs m (Maybe v)
lookup k = do
    tree <- get
    ((res, set), tree') <- inSandbox $ AVL.lookup (union # k) tree
    put tree'
    lift $ tell set
    return (join $ (^?union) <$> res)

-- | Lookup key in the global tree.
require
    :: forall k v h ks vs m
    .  AVL.Retrieves h ks vs m
    => Member k ks
    => Member v vs
    => Relates k v
    => (Show k, Typeable k)
    => k
    -> SandboxT h ks vs m v
require k = do
    lookup k
        >>= maybe (AVL.notFound k) return

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

data WrongOriginState = WrongOriginState
    { wosExpected :: String
    , wosGot      :: String
    }
    deriving stock    Show
    deriving anyclass Exception

data DivergedWithProof = DivergedWithProof
    { dwpExpected :: String
    , dwpGot      :: String
    }
    deriving stock    Show
    deriving anyclass Exception

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
            { wosExpected = show $ AVL.rootHash (AVL.unProof proof)
            , wosGot      = show $ AVL.rootHash  tree
            }

    let tree' = AVL.unProof proof
    AVL.append tree'

    ((res, tree''), _) <- runWriterT $ runStateT (interp tx) tree'

    unless (AVL.rootHash tree'' == endHash) $ do
        throw DivergedWithProof
            { dwpExpected = show $ endHash
            , dwpGot      = show $ AVL.rootHash tree''
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
            { wosExpected = show $ endHash
            , wosGot      = show $ AVL.rootHash tree
            }

    let tree' = AVL.unProof proof
    AVL.append tree'

    ((res, tree''), _) <- runWriterT $ runStateT (interp tx) tree'

    unless (AVL.rootHash tree'' == AVL.rootHash tree) $ do
        throw DivergedWithProof
            { dwpExpected = show $ AVL.rootHash tree
            , dwpGot      = show $ AVL.rootHash tree''
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

pair
  :: ( Member k keys
     , Member v values
     , Relates k v
     )
  => (k, v)
  -> (keys, values)
pair (k, v) = (union # k, union # v)

type LightNode h k v = StateT h

instance
    ( Show h
    , Ord k
    , Ord h
    , AVL.Hash h k v
    , MonadCatch m
    )
    => AVL.Retrieves h k v (LightNode h k v m)
  where
    retrieve = AVL.notFound

instance
    ( Show h
    , Ord k
    , Ord h
    , AVL.Hash h k v
    , MonadCatch m
    )
    => AVL.Appends h k v (LightNode h k v m)
  where
  getRoot = get
  setRoot = put
  massStore _kvs = do
    -- do nothing, we're client
    return ()

runOnLightNode :: h -> LightNode h k v m a -> m (a, h)
runOnLightNode initial action =
  runStateT action initial
