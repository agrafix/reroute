import qualified Web.Routing.Specs.TextRoutingSpec
import qualified Web.Routing.Specs.SafeRoutingSpec

import Test.Hspec

main :: IO ()
main =
    hspec $
    do Web.Routing.Specs.TextRoutingSpec.spec
       Web.Routing.Specs.SafeRoutingSpec.spec
