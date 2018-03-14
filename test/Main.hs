
import qualified Insertion
import qualified Proof
import qualified Lookup

import           Test.Framework (Test, defaultMain, testGroup)

main = defaultMain
    (   []
    ++  Proof.tests
    ++  Insertion.tests
    ++  Lookup.tests
    )
