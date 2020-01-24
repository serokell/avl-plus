
module NotLoosingHashes where

import Control.Lens (makePrisms)
import Control.Monad
import Control.Monad.Catch.Pure hiding (try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader
import Control.Monad.State

import Data.Foldable (for_)
import Data.Hashable (Hashable (hash))
import Data.Relation (Relates)
import Data.Traversable (for)
import Data.Union (Member (union))
import qualified Data.Map as Map

import Data.Blockchain.Storage.AVL
  ( Proven, apply, rollback
  , insert, {-delete, lookup,-} require
  , autoCommit, recordProof_
  , CacheT, Commit (..), try, genesis, pair
  {- , inCacheT -}
  )
import qualified Data.Tree.AVL as AVL
import qualified Data.Tree.AVL.Store.Pure.Sharded as Sharded
import qualified Data.Tree.AVL.Store.Pure as Pure (newState)

import GHC.Generics (Generic)

import Debug.Trace

type Name = String

{-
  We will define the keys for our storage. I choose such names for constructors
  because we use `Member` interface, which will recognise concrete keys as part
  of the whole `Keys` type. All keys should be declared that way.

  Also, I recommend making the keys `data` types or `newtypes` - otherwise type
  inference might get a bit brittle.

  For this example, I will use `Data.Hashable` to provide hash algorithm -
  that's why there is a deriving of `Hashable`. For industrial use, you'd
  better equip some hash algo from `cryptonite` package. Hash collision attack
  are no joke.
-}

data Keys
  = K1 Balance
  | K2 Friends
  deriving stock    (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

newtype Balance = Balance Name
  deriving stock   (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

newtype Friends = Friends Name
  deriving stock    (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

makePrisms ''Keys

instance Member Balance Keys where union = _K1
instance Member Friends Keys where union = _K2

{-
  The same goes for values (except for comparison instances). Concrete value
  types can be just anything.
-}

data Values
  = V1  Word
  | V2 [Name]
  deriving stock    (Show, Generic)
  deriving anyclass (Hashable)

makePrisms ''Values

instance Member  Word   Values where union = _V1
instance Member [Name]  Values where union = _V2

{-
  And the last bit - we help type inference by specifying which key type is
  bound to which value type. Under "help" I mean it won't work otherwise.
-}

instance Relates Balance  Word
instance Relates Friends [Name]

{-
  As I've said, we using the simplest hash available for our example.
-}

newtype Hash = Hash { getHash :: String }
  deriving stock   (Eq, Ord, Generic)
  deriving newtype (Hashable)

instance Show Hash where
  show (Hash h) = h

toBase64 :: Int -> String
toBase64
  = (++ ">")
  . ("<" ++)
  . concat
  . reverse
  . map     (\d -> color d (return (base64 !! d)))
  . take      4
  . map     (`mod` 16)
  . iterate (`div` 16)

color :: Int -> String -> String
color c s = "\ESC[" ++ show (30 + (mod c 8)) ++ (if c >= 8 then ";1" else "") ++ "m" ++ s ++ "\ESC[0m"

base64 :: String
base64 = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+/"

{-
  We plug that hash in. The best hash is one that has an instance for all
  standart types, or else you have to write 5 instances, for, let's say, quite
  mundane datatypes.
-}

instance Hashable a => AVL.ProvidesHash a Hash where
  getHash = Hash . toBase64 . hash

{-
  The datatypes are done, let's do it for action typess. There is no limitation
  of what transaction can be. You can even make a different types for different
  kinds of transactions.

  You can add gas costs or make it into some opcode VM, if you like.

  The block can be modelled as another type of transaction, where you apply
  "normal" transactions and check Proof-of-Work or Proof-of-Stake.

  The generation of Work/Stake proofs is out of scope for this library and this
  example. The "proof" in this text means "AVL+-proof", which is completely
  different thing.
-}

data Transaction
  = Pay       Name Name Word
  | AddFriend Name Name
   deriving stock (Eq, Show, Generic)

{-
  Another useful thing is exceptions, which could arise when you apply the
  transaction. We will define only one here (other being `NotFound`, which is
  already in the `avl-plus` package).
-}

data Overdraft = Overdraft
  { oWho        :: Name
  , oOnlyHas    :: Word
  , oTriedToPay :: Word
  }
  deriving (Show)

instance Exception Overdraft

{-
  Let's write an interpreter for our `Transaction` type. It has be a
  function that consumes a transaction and runs in a `CacheT` over any monad
  that can `Retrieve` parts of the tree from storage. The "any monad" part is
  crucial for allowing the _same_ interpreter to run on both light-clients and
  full-state clients.

  This way, you get `requre`, `lookup`, `insert` and `delete` operations which
  operate on the global map (the AVL+-tree). While in `CacheT`, you can't
  change the storage; to do so, you must feed the `CacheT` computation into
  either `autoCommit` or `manualCommit`. If you do so with `manualCommit`, to
  actually apply changes you need to call `commit` when you need it without any
  arguments inside `CacheT` context. The `manualCommit` is provided for the
  case you end with some pretty complex error recovery mechanism as a part of
  transaction application.

  I recommend sticking to `autoCommit`.

  Note: If you call `commit` in a block once, you cannot `autoCommit` this
  block, the compiler won't let you.

  In both cases, if an exception happens during evaluation, the state of the
  storage is restored to the one just before the call.

  Therefore, if at any point the evaluation cannot proceed, you pretty much
  just `throwM` an exception.
-}

interpret
  :: AVL.Retrieves Hash Keys Values m
  => Transaction
  -> CacheT c Hash Keys Values m ()
interpret tx = case tx of
  Pay from to (fromIntegral -> amount) -> AVL.while ("pay: " ++ show tx) do
    src  <- AVL.while ("getting balance from " ++ show from) $ require (Balance from)
    dest <- require (Balance to)

    unless (src >= amount) $ do
      throwM Overdraft
        { oWho        = from
        , oOnlyHas    = src
        , oTriedToPay = amount
        }

    insert (Balance from) (src  - amount)
    insert (Balance to)   (dest + amount)

  AddFriend friend to -> do
    src <- require (Friends to)
    _   <- require (Friends friend)   -- check if the friend exists

    insert (Friends to) (friend : src)

{-
  And we're done with the business-logic part!

  Let's see what can we do with all the things we declared above. The first
  opetation is to `recordProof` of the transaction, so it can be run on any
  node, even the one without any storage.

  The `recordProof` should only be run on full-state node, of course.

  The proof is an excerpt from the storage with a size < `O(log2(N) * Changes)`,
  where `Changes` is a count of key-value pairs that were either written or
  read. If there is 1 G of key-value pairs and 20 were changed, the proof would
  contain below 20 * 30 tree nodes.

  Recording produces a `Proven` transaction along with evaluation result (if
  any). The `Proven` datatype is exported openly and is `Generic`, so you can
  derive some serialisation.
-}

recordTxs
  :: AVL.Appends Hash Keys Values m
  => [Transaction]
  -> CacheT 'Auto Hash Keys Values m [Proven Hash Keys Values Transaction]
recordTxs txs = do
  for txs \tx -> do
    recordProof_ tx interpret

{-
  Second thing we can do is to apply the transaction. Note: we're not
  _commiting_ anything yet, the changes and new state are still in the cache.
-}

consumeTxs
  :: AVL.Appends Hash Keys Values m
  => [Proven Hash Keys Values Transaction]
  -> CacheT c Hash Keys Values m ()
consumeTxs txs = do
  for_ txs \tx -> do
    apply tx interpret

{-
  Third thing is to roll back; and yes, we do it with the same interpreter we
  _apply_ the transaction. The interpreter is needed because we need to be
  sure if the current state is reachable with the transaction from the point we
  rollback to.
-}

rollbackTxs
  :: AVL.Appends Hash Keys Values m
  => [Proven Hash Keys Values Transaction]
  -> CacheT c Hash Keys Values m ()
rollbackTxs txs = do
  for_ (reverse txs) \tx -> do
    rollback tx interpret

{-
  That's all we can do; lets define our nodes.

  The light-client (or just `Client`) goes first. It only stores the root hash
  of the AVL+-tree and can roll back via `Catch`. It can't read or write into
  storage, because there isn't any underneath it. So, we `throwM` on reads and
  ignore writes.
-}

type ClientNode = StateT Hash Catch

instance AVL.Retrieves Hash Keys Values ClientNode where
  retrieve = AVL.notFound

instance AVL.Appends Hash Keys Values ClientNode where
  getRoot = get
  setRoot = put
  massStore _kvs = do
    -- do nothing, we're client
    return ()

runOnClient :: Hash -> ClientNode a -> Either SomeException (a, Hash)
runOnClient initial action =
  runCatch do
    runStateT action initial

{-
  The full-state node (or `Server` node) definition is more mouthful.

  We will use Redis for simplicity, but any key-value storage can be used
  instead. You can even employ remote storage solution. After all, the inner
  scheme of storing prevents data corruption.
-}

type ServerNode = Sharded.StoreT Hash Keys Values IO

{-
  These two methods are used as a part of example test application to visualise
  the process.
-}

printState
  :: AVL.Appends Hash Keys Values m
  => MonadIO m
  => String
  -> Bool
  -> m ()
printState msg _dump = do
  root <- AVL.currentRoot

  liftIO do
    putStrLn ("  " ++ msg ++ ":")
    putStrLn ("    root hash: " ++ show (AVL.rootHash root))

  when _dump $ do
    liftIO do
      putStrLn  "    keys/values: "

    list <- AVL.toList root

    liftIO do
      for_ list \(k, v) ->
        putStrLn ("      " ++ show k ++ ": " ++ show v)

tryCase
  :: AVL.Appends Hash Keys Values m
  => MonadIO m
  => Show a
  => String
  -> CacheT 'Auto Hash Keys Values m a
  -> m (Maybe a)
tryCase msg action = do
  liftIO do
    putStrLn  ""
    putStrLn   msg
    putStrLn  "=="

  printState "before" False

  res <- try $ autoCommit action

  liftIO do
    putStrLn  "  result:"
    putStrLn ("    " ++ show res)

  printState "after" False

  return (either (const Nothing) Just res)

newtype Omitted a = Omitted a

-- -- dumpWholeTree :: (AVL.Appends Hash Keys Values m, MonadIO m) => m ()
-- dumpWholeTree = do
--   root <- inCacheT $ AVL.currentRoot
--   ((), set) <- inCacheT $ AVL.fold ((), flip const, id) root
--   tree <- inCacheT $ AVL.prune set root
--   liftIO $ putStrLn "###########################"
--   liftIO (print tree)
--   liftIO $ putStrLn "###########################"

instance Show (Omitted a) where
  show _ = "<omitted>"

{-
  Warning, this method wipes out the Redis database!

  We will test on two nodes, both of which are full-state Redis-based ones.

  Let's see, how does it work together:
-}
test :: ServerNode ()
test = do
  {-
    First, we need to initialise both nodes with identical data.
  -}
  let
    initialData =
      [ pair (Balance "Petua",   200)
      , pair (Friends "Petua",   [])

      , pair (Balance "Vasua",   300)
      , pair (Friends "Vasua",   [])

      , pair (Balance "Maloney", 10)
      , pair (Friends "Maloney", ["Kesha"])
      ]

  Sharded.withShard "test1" do
    genesis initialData

  Sharded.withShard "test2" do
    genesis initialData

  {-
    We can start recording transactions now (transaction recorded is being
    applied at the same time).

    Note: no morally ambiguos thing is happening in the transaction.
  -}
  Just ptxs <- Sharded.withShard "test1" do
    tryCase "Making good txs (1)" do
      recordTxs
        [ Pay       "Vasua" "Petua" 200
        , AddFriend "Vasua" "Petua"
        ]

  _ <- Sharded.withShard "test2" do
    tryCase "Applying good txs (2)" do
      consumeTxs ptxs

  liftIO $ print "HERE"

  ptxs' <- Sharded.withShard "test1" $ do
    autoCommit do
      recordTxs
        [ Pay       "Vasua"   "Petua" 100
        , AddFriend "Maloney" "Petua"
        ]

  {-
    TODO: find why the following block fails.
  -}

  traceShowM ptxs'

  Sharded.withShard "test2" do
    tryCase "Applying other txs (2)" do
      consumeTxs ptxs'

  Sharded.withShard "test1" do
    tryCase "Rolling back wrong txs (1)" do
      rollbackTxs ptxs

  Nothing <- Sharded.withShard "test1" do
    tryCase "Making bad txs#1 (1)" do
      recordTxs
        [ Pay       "Maloney" "Petua" 200
        , AddFriend "Vasua"   "Petua"
        ]

    tryCase "Making bad txs#2 (2)" do
      recordTxs
        [ Pay       "Maloney" "Petua" 10
        , AddFriend "Haxxor"  "Petua"
        ]

  Sharded.withShard "test1" do
    tryCase "Rolling back good txs (1)" do
      rollbackTxs ptxs'

  Sharded.withShard "test1" do
    tryCase "Rolling back now good txs (1)" do
      rollbackTxs ptxs

  return ()

main :: IO ()
main = do
  let shards = ["test1", "test2"]
  stores <- for shards \_ -> do
    Pure.newState
  test `runReaderT` (Map.fromList $ zip shards stores)