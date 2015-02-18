
import Test.Framework

import qualified Application
import qualified Method

main :: IO ()
main = defaultMain
    [ Application.test
    , Method.test
    ]
