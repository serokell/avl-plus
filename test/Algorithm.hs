module Algorithm (tests) where

import Common

import qualified Data.Tree.AVL as AVL

tests :: Spec
tests = describe "Algorithms" $ do
    describe "WithBounds" $ do
        it "forall a . Bottom < Plain a < Top" $ \a ->
            AVL.Bottom < AVL.Plain a && AVL.Plain a < AVL.Top
