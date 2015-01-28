
import Test.Framework

import Application
import Method
import Tree

main :: IO ()
main = defaultMain
    [ methodTests
    , applicationTests
    , testTree
    ]

