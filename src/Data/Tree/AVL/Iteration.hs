
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Tree.AVL.Iteration where

import Control.Exception
import Control.Lens
import Control.Monad.Catch
import Control.Monad.State

import Data.Proxy
import Data.Tree.AVL.Internal
import Data.Typeable

-- | Stores return chain for the iteration.
--   Each list element represents direction it took and node it came to.
newtype IteratedT h m a = IteratedT { getIteratedT :: StateT [Either h h] m a }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadState [Either h h]
        , MonadThrow
        , MonadCatch
        )

-- | Possible situations to handle during iteration.
data IteratonError
    = ReturnStackIsEmpty
    | NoLeftBranch
    | NoRightBranch
    deriving (Show, Typeable)

instance Exception IteratonError

instance KVStoreMonad h m => KVStoreMonad h (IteratedT h m) where
    store  k = IteratedT . lift . store k
    retrieve = IteratedT . lift . retrieve

class CanIterateAVL h k v m where
    startIteration    :: Map h k v -> m ()
    continueIteration :: Proxy h -> m (Maybe (k, v))

instance Stores h k v m => CanIterateAVL h k v (IteratedT h m) where
    startIteration      = start
    continueIteration _ = nextKV

-- instance (CanIterateAVL h k v m, Monad m, MonadTrans t) => CanIterateAVL h k v (t m) where
--     startIteration    = lift . startIteration
--     continueIteration = lift . continueIteration

runIteratedT :: Monad m => IteratedT h m a -> m a
runIteratedT (IteratedT action) = evalStateT action []

start :: forall h k v m . Stores h k v m => Map h k v -> IteratedT h m ()
start tree = do
    push $ Left $ rootHash tree
    _ :: Map h k v <- leftmostKVNode
    return ()

leftmostKVNode :: forall h k v m . Stores h k v m => IteratedT h m (Map h k v)
leftmostKVNode = do
    getKV >>= \case
        Just (_ :: (k, v)) ->
            shallowBody

        Nothing -> do
            _ :: Map h k v <- goLeft
            leftmostKVNode

nextKVNode :: forall h k v m . Stores h k v m => IteratedT h m (Map h k v)
nextKVNode = do
    peek >>= \case
        Left _ -> do
            _ :: Map h k v <- goUp
            _ :: Map h k v <- goRight
            leftmostKVNode

        Right _ -> do
            _ :: Map h k v <- goUp
            nextKVNode

nextKV :: forall h k v m . Stores h k v m => IteratedT h m (Maybe (k, v))
nextKV = do
    _ :: Map h k v <- nextKVNode
    getKV

getKV :: Stores h k v m => IteratedT h m (Maybe (k, v))
getKV = do
    subtree <- body
    return $ do
        k <- subtree^?mlKey
        v <- subtree^?mlValue
        return (k, v)

goLeft :: Stores h k v m => IteratedT h m (Map h k v)
goLeft = do
    subtree <- body
    case subtree^?mlLeft of
        Just left -> do
            push $ Left $ rootHash left
            return left

        Nothing -> do
            throwM NoLeftBranch

goRight :: Stores h k v m => IteratedT h m (Map h k v)
goRight = do
    subtree <- body
    case subtree^?mlRight of
        Just right -> do
            push $ Right $ rootHash right
            return right

        Nothing -> do
            throwM NoRightBranch

goUp :: Stores h k v m => IteratedT h m (Map h k v)
goUp = do
    _ <- pop
    shallowBody

body :: Stores h k v m => IteratedT h m (MapLayer h k v (Map h k v))
body = open =<< shallowBody

shallowBody :: Stores h k v m => IteratedT h m (Map h k v)
shallowBody = ref . onlyHash <$> peek

push :: KVStoreMonad h m => Either h h -> IteratedT h m ()
push h = modify (h :)

peek :: KVStoreMonad h m => IteratedT h m (Either h h)
peek = do
    get >>= \case
        []    -> do
            throwM ReturnStackIsEmpty

        h : _ -> do
            return h

pop :: KVStoreMonad h m => IteratedT h m (Either h h)
pop = do
    h <- peek
    modify tail
    return h

onlyHash :: Either h h -> h
onlyHash = either id id
