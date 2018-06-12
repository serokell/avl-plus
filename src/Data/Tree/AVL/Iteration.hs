
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Tree.AVL.Iteration where

import Control.Exception (Exception)
import Control.Lens
import Control.Monad.Catch
import Control.Monad.State.Strict

import Data.Default
import Data.Proxy
import Data.Tree.AVL.Internal
import Data.Typeable

import Prelude hiding (break)

data IterationState h = IterationState
    { stack    :: [Either h h]
    , isBroken :: Bool
    }

instance Default (IterationState h) where
    def = IterationState [] False

instance Show h => Show (IterationState h) where
    show (IterationState stack isBroken)
        | isBroken  = "It broke"
        | otherwise = map bit stack
      where
        bit = either (const 'L') (const 'R')

class KVStoreMonad h m => ProvidesIterationState h m where
    getIterationState :: m (IterationState h)
    putIterationState :: IterationState h -> m ()

    modifyIterationState :: (IterationState h -> IterationState h) -> m ()
    modifyIterationState f = putIterationState . f =<< getIterationState

    getsIterationState :: (IterationState h -> a) -> m a
    getsIterationState f = do
        s <- getIterationState
        return (f s)

type StoresAndIterates h k v m = (Stores h k v m, ProvidesIterationState h m)

-- | Stores return chain for the iteration.
--   Each list element represents direction it took and node it came to.
newtype IteratedT h m a = IteratedT { getIteratedT :: StateT (IterationState h) m a }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadState (IterationState h)
        , MonadThrow
        , MonadCatch
        , MonadIO
        )

instance KVStoreMonad h m => ProvidesIterationState h (IteratedT h m) where
    getIterationState = IteratedT get
    putIterationState = IteratedT . put

-- | Possible situations to handle during iteration.
data IterationError
    = ReturnStackIsEmpty
    | NoLeftBranch
    | NoRightBranch
    deriving (Show, Typeable)

instance Exception IterationError

instance KVStoreMonad h m => KVStoreMonad h (IteratedT h m) where
    store  k = IteratedT . lift . store k
    retrieve = IteratedT . lift . retrieve

class CanIterateAVL h k v m where
    startIteration    :: Map h k v -> m ()
    continueIteration :: Proxy h -> m (Maybe (k, v))

instance Stores h k v m => CanIterateAVL h k v (IteratedT h m) where
    startIteration          = start
    continueIteration proxy = nextKV proxy

-- instance (CanIterateAVL h k v m, Monad m, MonadTrans t) => CanIterateAVL h k v (t m) where
--     startIteration    = lift . startIteration
--     continueIteration = lift . continueIteration

runIteratedT :: Monad m => IteratedT h m a -> m a
runIteratedT (IteratedT action) = evalStateT action (IterationState [] False)

start :: forall h k v m . StoresAndIterates h k v m => Map h k v -> m ()
start tree = do
    push $ Left $ rootHash tree
    _ :: Map h k v <- leftmostKVNode
    return ()

leftmostKVNode :: forall h k v m . StoresAndIterates h k v m => m (Map h k v)
leftmostKVNode = do
    getKV (Proxy :: Proxy h) >>= \case
        Just (_ :: (k, v)) ->
            shallowBody

        Nothing -> do
            _ :: Map h k v <- goLeft
            leftmostKVNode
  `catch` \NoLeftBranch ->
    shallowBody

nextKVNode :: forall h k v m . StoresAndIterates h k v m => m (Map h k v)
nextKVNode = do
    -- mkv :: Maybe (k, v) <- getKV (Proxy :: Proxy h)
    -- s :: IterationState h <- getIterationState
    -- liftIO $ putStrLn $ show s ++ " " ++ show mkv
    peek >>= \case
        Left (_ :: h) -> do
            _ :: Map h k v <- goUp
            _ :: Map h k v <- goRight
            leftmostKVNode

        Right (_ :: h) -> do
            _ :: Map h k v <- goUp
            nextKVNode
  `catch` \ReturnStackIsEmpty -> do
    break (Proxy :: Proxy h)
    shallowBody

nextKV :: forall h k v m . StoresAndIterates h k v m => Proxy h -> m (Maybe (k, v))
nextKV _ = do
    broken <- getsIterationState $ \is -> isBroken (is :: IterationState h)

    if broken
    then do
        return Nothing

    else do
        res <- getKV (Proxy :: Proxy h)
        _ :: Map h k v <- nextKVNode
        return res


getKV :: forall h k v m . StoresAndIterates h k v m => Proxy h -> m (Maybe (k, v))
getKV _ = do
    subtree :: MapLayer h k v (Map h k v) <- body
    return $ do
        k <- subtree^?mlKey
        v <- subtree^?mlValue
        return (k, v)

goLeft :: StoresAndIterates h k v m => m (Map h k v)
goLeft = do
    subtree <- body
    case subtree^?mlLeft of
        Just left -> do
            push $ Left $ rootHash left
            return left

        Nothing -> do
            throwM NoLeftBranch

goRight :: StoresAndIterates h k v m => m (Map h k v)
goRight = do
    subtree <- body
    case subtree^?mlRight of
        Just right -> do
            push $ Right $ rootHash right
            return right

        Nothing -> do
            throwM NoRightBranch

goUp :: forall h k v m . StoresAndIterates h k v m => m (Map h k v)
goUp = do
    _ :: Either h h <- pop
    shallowBody

body :: StoresAndIterates h k v m => m (MapLayer h k v (Map h k v))
body = open =<< shallowBody

shallowBody :: StoresAndIterates h k v m => m (Map h k v)
shallowBody = ref . onlyHash <$> peek

push :: ProvidesIterationState h m => Either h h -> m ()
push h = modifyIterationState $ \is -> is { stack = h : stack is }

peek :: ProvidesIterationState h m => m (Either h h)
peek = do
    getsIterationState stack >>= \case
        []    -> do
            throwM ReturnStackIsEmpty

        h : _ -> do
            return h

pop :: forall h m . ProvidesIterationState h m => m (Either h h)
pop = do
    h <- peek
    modifyIterationState $ \is ->
        is { stack = tail $ stack is }
            :: IterationState h
    return h

onlyHash :: Either h h -> h
onlyHash = either id id

break :: forall h m . ProvidesIterationState h m => Proxy h -> m ()
break _ = modifyIterationState $ \is ->
    is { isBroken = True }
        :: IterationState h
