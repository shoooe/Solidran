module Solidran.Subs.DetailSpec (spec) where

import Test.Hspec
import Solidran.Subs.Detail

spec :: Spec
spec = do
    describe "Solidran.Subs.Detail" $ do
        describe "findOccs" $ do
            it "should work on the given sample" $ do
                findOccs "ATAT" "GATATATGCATATACTT"
                    `shouldBe` [2, 4, 10]
            it "should work on any list" $ do
                findOccs [1, 2] [1, 2, 4, 1, 1, 2]
                    `shouldBe` [1, 5]
            it "should return an empty list on empty haystack" $ do
                findOccs [1, 2] [] `shouldBe` []
                findOccs ([] :: [Int]) [] `shouldBe` []
            it "should return an empty list on empty needle" $ do
                findOccs [] [1, 2, 4] `shouldBe` []
            it "should return an empty list on no matches" $ do
                findOccs [1] [4, 3, 6, 7] `shouldBe` []
                findOccs [7, 34] [2, 34, 7, 1] `shouldBe` []
