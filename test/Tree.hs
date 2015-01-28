module Tree(testTree) where

import Data.Apiary.Tree
import Test.Framework
import Test.Framework.Providers.QuickCheck
import Unsafe.Coerce

mkTestTree :: Int -> Tree
mkTestTree i = foldr cons Tip $ map unsafeCoerce [0..i]

testTreeToList :: Int -> Tree -> [Int]
testTreeToList l t = foldr (\i b -> unsafeCoerce (t `index` i) : b) [] [0..l]

testTree :: Test
testTree = testProperty "Data.Apiary.Tree" $ \len ->
    testTreeToList len (mkTestTree len) == [0..len]
