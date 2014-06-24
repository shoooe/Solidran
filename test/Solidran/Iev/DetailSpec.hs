module Solidran.Iev.DetailSpec (spec) where

import Test.Hspec
import Solidran.Iev.Detail

spec :: Spec
spec = do
    describe "Solidran.Iev.Detail" $ do
        describe "expectedDom" $ do
            it "should return 0 on 0 organisms" $ do
                expectedDom (0, 0, 0, 0, 0, 0) `shouldBe` 0.0
            it "should return the double of the organisms in some cases" $ do
                expectedDom (10, 0, 0, 0, 0, 0) `shouldBe` 20.0
                expectedDom (0, 13, 0, 0, 0, 0) `shouldBe` 26.0
                expectedDom (0, 0, 54, 0, 0, 0) `shouldBe` 108.0
                expectedDom (97, 455, 1, 0, 0, 0) `shouldBe` 1106.0
            it "should work on the given example" $ do
                expectedDom (1, 0, 0, 1, 0, 1) `shouldBe` 3.5
            it "should work in other cases" $ do
                expectedDom (3, 5, 0, 1, 54, 7) `shouldBe` 71.5
                expectedDom (45, 6, 0, 0, 1, 0) `shouldBe` 103.0
                expectedDom (0, 0, 4, 5, 60, 12) `shouldBe` 75.5
