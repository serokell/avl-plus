module Lookup (tests) where

import Data.Map as Map (fromList)
import Data.Set as Set (member)

import Common

import qualified Data.Tree.AVL as AVL

tests :: Spec
tests = describe "Lookup" $ do
    it' "Lookup actually works" $ \(list) -> do
        case uniqued list of
          [] -> do
            return True

          (k, v) : rest -> do
            tree               <- AVL.fromList $ (k, v) : rest
            ~((Just v1, _), _) <- AVL.lookup k tree

            return (v == v1)

    it' "LookupMany actually works" $ \(ks, list) -> do
        tree                <- AVL.fromList list
        ((selection, _), _) <- AVL.lookupMany ks tree

        let lookupMany =  Map.fromList . filter ((`Set.member` ks) . fst)

        return (selection == lookupMany list)

    describe "Proofs" $ do
        it' "Generated proofs are verified" $ \(k, list) -> do
            tree           <- AVL.fromList list
            ((_, set0), _) <- AVL.lookup k tree

            AVL.checkProof (AVL.rootHash tree) <$> AVL.prune set0 tree

        it' "Generated proofs are replayable" $ \(k, list) -> do
            tree              <- AVL.fromList list
            ((res,  set0), _) <- AVL.lookup k tree
            ((res1, set1), _) <- AVL.lookup k . AVL.unProof =<< AVL.prune set0 tree

            return $ res   == res1
                  && set0  == set1
