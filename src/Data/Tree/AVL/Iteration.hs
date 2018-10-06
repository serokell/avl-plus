-- | AVL tree iteration.
module Data.Tree.AVL.Iteration
    ( walkDFS
    , foldIf
    , fold
    ) where

import Control.Lens ((^.))
import Control.Monad.Writer.Strict (WriterT (runWriterT), lift, tell)
import qualified Data.Set as Set

import Data.Tree.AVL.Internal

-- | Traverses the tree in DFS manner meeting keys in ascending order.
walkDFS
  :: forall h k v m b res
  .  Retrieves h k v m
  =>  ( b
      , MapLayer h k v (Maybe h) -> b -> b
      , b -> res
      )
  -> Map h k v
  -> m (res, Set.Set Revision)
walkDFS (start, add, finish) root = runWriterT $ finish <$> go start root
  where
    go :: b -> Map h k v -> WriterT (Set.Set Revision) m b
    go acc mapping =
        lift (load mapping) >>= \point -> do
            tell (Set.singleton (point^.mlRevision))
            let point' = rootHash <$> point
            case point of
                MLBranch { _mlLeft = l, _mlRight = r } -> do
                  acc' <- go (add point' acc) l
                  go acc' r
                MLLeaf {} -> return $ add point' acc
                MLEmpty {} -> return acc

-- | Left-to-right fold.
foldIf
    :: forall h k v m b res
    .  Retrieves h k v m
    => ( k -> Bool
       , b
       , (k, v) -> b -> b
       , b -> res
       )
    -> Map h k v
    -> m (res, Set.Set Revision)
foldIf (good, start, add, finish) = walkDFS (start, collectKVAnd add, finish)
  where
    collectKVAnd :: ((k, v) -> b -> b) -> MapLayer h k v (Maybe h) -> b -> b
    collectKVAnd act = \case
        MLLeaf { _mlKey = (unsafeFromWithBounds -> k), _mlValue = v }
            | good k -> act (k, v)
        _other -> id

-- | Left-to-right fold.
fold
    :: Retrieves h k v m
    => ( b
       , (k, v) -> b -> b
       , b -> res
       )
    -> Map h k v
    -> m (res, Set.Set Revision)
fold (start, add, finish) = foldIf (\_ -> True, start, add, finish)
