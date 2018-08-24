
import qualified Algorithm
import qualified Insertion
import qualified Iteration
import qualified Proof
import qualified Lookup

import Common

main :: IO ()
main = hspec $ do
    describe "AVL+ tree" $ do
        Iteration.tests
        Algorithm.tests
        Proof.tests
        Insertion.tests
        Lookup.tests
