
import qualified Algorithm
import qualified Insertion
import qualified Deletion
import qualified Lookup

import Common

main :: IO ()
main = hspec $ do
    describe "AVL+ tree" $ do
        Deletion.tests
        Insertion.tests
        Lookup.tests
        Algorithm.tests
