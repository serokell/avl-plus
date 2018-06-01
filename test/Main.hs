
import qualified Algorithm
import qualified Insertion
import qualified Proof
import qualified Lookup
import qualified Cache

import           Test.Framework (defaultMain)

main :: IO ()
main = defaultMain
    (   []
    ++  Algorithm.tests
    ++  Proof.tests
    ++  Insertion.tests
    ++  Lookup.tests
    ++  Cache.tests
    )
