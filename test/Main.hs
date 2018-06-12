
-- import qualified Algorithm
-- import qualified Insertion
import qualified Iteration
-- import qualified Proof
-- import qualified Lookup
-- import qualified Cache

import           Test.Framework (defaultMain)

main :: IO ()
main = defaultMain
    (   []
    ++  Iteration.tests
    -- ++  Algorithm.tests
    -- ++  Proof.tests
    -- ++  Insertion.tests
    -- ++  Lookup.tests
    -- ++  Cache.tests
    )
