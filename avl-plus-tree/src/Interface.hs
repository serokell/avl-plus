
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language GADTs #-}
{-# language DeriveFunctor #-}
{-# language StandaloneDeriving #-}

module Interface where

import Control.Monad.State.Strict

import AVLPlusTree
import Prover

data Command k v a where
    Insert :: v -> Command k v ()
    Delete :: k -> Command k v ()
    Batch  :: [Command k v ()] -> Query k v b -> Command k v b

data Query k v a where
    Lookup :: k -> Query k v v
    Noop   ::      Query k v ()

class RunCommand h k v m | k v -> h where
    type Proof h :: *
    runCommand :: Command k v a -> m (a, [Proof h])

instance
    ( MonadState (Map h k v) m
    , Hash h k v )
      =>
    RunCommand h k v m
  where
    type Proof h = KeyProof h
