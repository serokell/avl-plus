
import qualified Algorithm
import qualified Insertion
import qualified Deletion
import qualified Lookup
import qualified Unsafe

import Common

main :: IO ()
main = hspec $ do
    describe "AVL+ tree" $ do
        Algorithm.tests
        Insertion.tests
        Deletion.tests
        Lookup.tests
        Unsafe.tests
