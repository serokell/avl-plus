
import qualified Insertion
import qualified Proof

import           Test.Framework (Test, defaultMain, testGroup)

main = defaultMain
    (   []
    ++  Proof.tests
    ++  Insertion.tests
    )
