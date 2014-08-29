
import Test.Framework

import Application
import Method

main :: IO ()
main = defaultMain
    [ methodTests
    , applicationTests
    ]

