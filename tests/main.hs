
import Test.Tasty

import qualified Application
import qualified Method

main :: IO ()
main = defaultMain $ testGroup "/"
    [ Application.test
    , Method.test
    ]
