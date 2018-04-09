
import qualified Insertion
import qualified Proof
import qualified Lookup
import qualified Cache

import           Test.Framework (defaultMain)

main :: IO ()
main = defaultMain
    (   []
    ++  Proof.tests
    ++  Insertion.tests
    ++  Lookup.tests
    ++  Cache.tests
    )
