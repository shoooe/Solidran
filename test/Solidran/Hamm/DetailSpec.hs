module Solidran.Hamm.DetailSpec (spec) where

import Test.Hspec
import Solidran.Hamm.Detail

spec :: Spec
spec = do
    describe "Solidran.Hamm.Detail" $ do
        describe "hammingDist" $ do
            it "should work in the given sample" $ do
                hammingDist "GAGCCTACTAACGGGAT" "CATCGTAATGACGGCCT"
                    `shouldBe` 7
            it "should work on empty strings" $ do
                hammingDist "" ""
                    `shouldBe` 0
            it "should work with any character" $ do
                hammingDist "333yg!.u=)8GYGU3¥~" "/^ayg?.u=)8gYGU3¥~"
                    `shouldBe` 5
                hammingDist "%&\"lqyYYUIhCDX%°" "%&'lqyYYUIhCDX%°"
                    `shouldBe` 1
