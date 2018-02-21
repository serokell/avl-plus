
{-# language GADTs #-}
{-# language TemplateHaskell #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}

module Data.Tree.Interface
  ( Operation(..)

  , Action(..)
  , generate
  , verify
  , perform

  , ValidationError

  , MI
  , Full, getFull
  , Headless, getHeadless
  )
  where

import Control.Lens               (makeLenses, use, uses, zoom, (^.), (.=))
import Control.Monad.State.Strict (State, state, put, modify, get)
import Control.Monad.Except       (ExceptT, throwError)
import Control.Monad              (when)

import Data.Tree.AVL

data Operation k v
  = Upsert k v
  | Delete k

data Action h k v = Action
    { _aOperation :: Operation k v
    , _aProof     :: Proof     h k v
    , _aDigest    :: h
    }

data ValidationError
    = WrongInputDigest
    | OutputDigestsDiverge

type MI s h k v = ExceptT ValidationError (State (s h k v))

newtype Full     h k v = Full     { _getFull     :: Map h k v }
newtype Headless h k v = Headless { _getHeadless :: h }

makeLenses ''Full
makeLenses ''Headless
makeLenses ''Action

generate :: Hash h k v => Operation k v -> MI Full h k v (Action h k v)
generate operation = case operation of
  Upsert k v -> generateWith (insert k v)
  Delete k   -> generateWith (delete k)

  where
    generateWith act = do
        proof       <- getFull `zoom` state act
        finalDigest <- use (getFull.rootHash)

        return Action
          { _aOperation = operation
          , _aProof     = proof
          , _aDigest    = finalDigest
          }

verify :: Hash h k v => Action h k v -> MI Headless h k v ()
verify action = case action^.aOperation of
  Upsert k v -> verifyWith (insertWithNoProof k v)
  Delete k   -> verifyWith (deleteWithNoProof k)

  where
    verifyWith act = do
        let proof @ (Proof stub) = action^.aProof

        yes <- uses getHeadless (`checkProof` proof)

        when (not yes) $ do
            throwError WrongInputDigest

        let stub1 = act stub
        let yes1  = checkProof (action^.aDigest) (Proof stub1)

        when (not yes1) $ do
            throwError OutputDigestsDiverge

        getHeadless .= action^.aDigest

perform :: Hash h k v => Action h k v -> MI Full h k v ()
perform action = case action^.aOperation of
  Upsert k v -> performWith (insertWithNoProof k v)
  Delete k   -> performWith (deleteWithNoProof k)

  where
    performWith act = do
        let proof = action^.aProof

        yes <- uses (getFull.rootHash) (`checkProof` proof)

        when (not yes) $ do
            throwError WrongInputDigest

        state0 <- get

        getFull `zoom` modify act

        final <- use (getFull.rootHash)

        when (final /= action^.aDigest) $ do
            put state0
            throwError $ OutputDigestsDiverge
