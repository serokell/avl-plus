
{- | High-level interface to AVL+-based blockchain storage, with capabilities
     to prove and verify transactions.

-}

module Data.Blockchain.Storage.AVL
    ( -- * Transaction wrapper
      Proven
    , record
    , record_
    , apply
    , rollback

      -- * Cache transformer
    , CacheT
    , insert
    , delete
    , lookup
    , require

      -- * Exceptions
    , WrongOriginState (..)
    , DivergedWithProof (..)

      -- * Transactions
    , manualCommit
    , commit
    , tryAutoCommit
    , autoCommit

      -- * Light node
    , LightNode
    , runOnLightNode

      -- * Helpers
    , pair
    , AVL.genesis

      -- * Database interfaces to implement
    , AVL.Retrieves (..)
    , AVL.Appends   (..)
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
import Data.Union
import Data.Relation

import GHC.Generics (Generic)

-- | Insert key/value into the blockchain state.
insert
    :: forall k v h ks vs c m
    .  ( AVL.Retrieves h ks vs m
       , Member k ks
       , Member v vs
       , Relates k v
       )
    => k
    -> v
    -> CacheT c h ks vs m ()
insert k v = do
    tree <- get
    (set, tree') <- inCacheT $ AVL.insert (union # k) (union # v) tree
    put tree'
    tell set

-- | Delete key from the blockchain state.
delete
    :: forall k h ks vs c m
    .  ( AVL.Retrieves h ks vs m
       , Member k ks
       )
    => k
    -> CacheT c h ks vs m ()
delete k = do
    tree <- get
    (set, tree') <- inCacheT $ AVL.delete (union # k) tree
    put tree'
    tell set

-- | Lookup key in the blockchain state.
lookup
    :: forall k v h ks vs c m
    .  ( AVL.Retrieves h ks vs m
       , Member k ks
       , Member v vs
       , Relates k v
    )
    => k
    -> CacheT c h ks vs m (Maybe v)
lookup k = do
    tree <- get
    ((res, set), tree') <- inCacheT $ AVL.lookup (union # k) tree
    put tree'
    tell set
    return (join $ (^?union) <$> res)

-- | Lookup key in the the blockchain state.
require
    :: forall k v h ks vs c m
    .  ( AVL.Retrieves h ks vs m
       , Member k ks
       , Member v vs
       , Relates k v
       , Show k
       )
    => k
    -> CacheT c h ks vs m v
require k = do
    lookup k
        >>= maybe (AVL.notFound k) return

-- | Perform a transaction with an interpreter, commit changes into the storage,
--   return the result of the transaction and the transaction, equipped with
--   a proof.
record
    :: AVL.Retrieves h k v m
    => tx
    -> (tx -> CacheT c h k v m a)
    -> CacheT c h k v m (a, Proven h k v tx)
record tx interp = do
    old        <- get
    (res, set) <- listen   $ interp tx
    proof      <- inCacheT $ AVL.prune set old
    new        <- get

    return (res, Proven tx proof (AVL.rootHash new))

-- | Same as the `record`, but the result of the transaction is @()@.
record_
    :: AVL.Retrieves h k v m
    => tx
    -> (tx -> CacheT c h k v m ())
    -> CacheT c h k v m (Proven h k v tx)
record_ = ((snd <$>) .) . record

-- | Thrown if the state the proven transaction originates is not what
--   it expects.
data WrongOriginState = WrongOriginState
    { wosExpected :: String
    , wosGot      :: String
    }
    deriving stock    Show
    deriving anyclass Exception

-- | Thrown if the state the proven transaction ends with is not the one
--   captured in the proof.
data DivergedWithProof = DivergedWithProof
    { dwpExpected :: String
    , dwpGot      :: String
    }
    deriving stock    Show
    deriving anyclass Exception

-- | The sandbox transformer to run `insert`, `delete` and `lookup` in.
newtype CacheT (c :: Commit) h k v m a = CacheT { unCacheT :: StateT (AVL.Map h k v) (WriterT (Set.Set h) m) a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState  (AVL.Map h k v)
    , MonadWriter (Set.Set h)
    , MonadThrow
    , MonadCatch
    , MonadIO
    )

data Commit
  = Auto
  | Manual

inCacheT :: (Ord h, Monad m) => m a -> CacheT c h k v m a
inCacheT = CacheT . lift . lift

runCacheT
    :: AVL.Retrieves h k v m
    => CacheT c h k v m a
    -> AVL.Map h k v
    -> m (a, AVL.Map h k v)
runCacheT action seed = do
    ((a, s), _) <- runWriterT $ unCacheT action `runStateT` seed
    return (a, s)

-- | Transaction @tx@ with a proof.
data Proven h k v tx = Proven
    { pTx      :: tx
    , pProof   :: AVL.Proof h k v
    , pEndHash :: h
    }
    deriving (Eq, Show, Generic)

-- | Run a transaction that has a proof.
--
--   Can be peformed on any node; it doesn't require that node to store
--   the full network state.
apply
    :: AVL.Retrieves h k v m
    => Proven h k v tx
    -> (tx -> CacheT c h k v m a)
    -> CacheT c h k v m a
apply (Proven tx proof endHash) interp = do
    tree <- get

    unless (AVL.rootHash tree `AVL.checkProof` proof) $ do
        throwM WrongOriginState
            { wosExpected = show $ AVL.rootHash (AVL.unProof proof)
            , wosGot      = show $ AVL.rootHash  tree
            }

    let tree' = AVL.unProof proof
    put tree'

    res    <- interp tx
    tree'' <- get

    unless (AVL.rootHash tree'' == endHash) $ do
        throw DivergedWithProof
            { dwpExpected = show $ endHash
            , dwpGot      = show $ AVL.rootHash tree''
            }

    return res

-- | Rollback the transaction that has a proof.
--
--   Can be peformed on any node; it doesn't require that node to store
--   the full blockchain state.
--
--   Only the last applied transaction can be rolled back.
rollback
    :: AVL.Retrieves h k v m
    => Proven h k v tx
    -> (tx -> CacheT c h k v m a)
    -> CacheT c h k v m a
rollback (Proven tx proof endHash) interp = do
    tree <- get

    unless (AVL.rootHash tree == endHash) $ do
        throw WrongOriginState
            { wosExpected = show $ endHash
            , wosGot      = show $ AVL.rootHash tree
            }

    let tree' = AVL.unProof proof
    put tree'

    res    <- interp tx
    tree'' <- get

    unless (AVL.rootHash tree'' == AVL.rootHash tree) $ do
        throw DivergedWithProof
            { dwpExpected = show $ AVL.rootHash tree
            , dwpGot      = show $ AVL.rootHash tree''
            }

    put tree'

    return res

commit :: AVL.Appends h k v m => CacheT 'Manual h k v m ()
commit = do
    tree <- get
    inCacheT $ AVL.append tree

-- | Run action, restore the blockchain state if it fails.
manualCommit
    :: forall e h k v m a
    .  (Exception e, AVL.Appends h k v m)
    => CacheT 'Manual h k v m a
    -> m (Either e a)
manualCommit action = do
    saved <- AVL.currentRoot
    do
        (res, new) <- runCacheT action saved
        AVL.append new
        return (Right res)
      `catch` \e -> do
        AVL.append saved
        return (Left e)

-- | Run action, restore the blockchain state if it fails.
tryAutoCommit
    :: forall h k v m a
    .  (AVL.Appends h k v m)
    => CacheT 'Auto h k v m a
    -> m (Either SomeException a)
tryAutoCommit action = do
    saved <- AVL.currentRoot
    do
        (res, new) <- runCacheT action saved
        AVL.append new
        return (Right res)
      `catch` \e -> do
        AVL.append saved
        return (Left e)

-- | As `transact`, rethrows the exception.
autoCommit
    :: forall h k v m a
    .  (AVL.Appends h k v m)
    => CacheT 'Auto h k v m a
    -> m a
autoCommit action = do
    tryAutoCommit action >>= either throwM return

-- | Helper to generate a list for `genesis`.
pair
  :: ( Member k keys
     , Member v values
     , Relates k v
     )
  => (k, v)
  -> (keys, values)
pair (k, v) = (union # k, union # v)

-- | An implementation for the light node.
--
--   Only stores the last root hash.
newtype LightNode h k v m a = LightNode { runLightNode :: StateT h m a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadThrow
    , MonadCatch
    , MonadIO
    )

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
  getRoot = LightNode get
  setRoot = LightNode . put
  massStore _kvs = do
    -- do nothing, we're client
    return ()

-- |
runOnLightNode :: h -> LightNode h k v m a -> m (a, h)
runOnLightNode initial action =
  runStateT (runLightNode action) initial
