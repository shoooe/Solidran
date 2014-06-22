module Solidran.Fibd.DetailSpec (spec) where

import Test.Hspec
import Solidran.Fibd.Detail

spec :: Spec
spec = do
    describe "Solidran.Fibd.Detail" $ do
        describe "countRabbits" $ do
            it "after 1 month, the count should be 1" $ do
                countRabbits 1 1 `shouldBe` 1
            it "should work on every step of the sample sequence" $ do
                countRabbits 1 3 `shouldBe` 1
                countRabbits 2 3 `shouldBe` 1
                countRabbits 3 3 `shouldBe` 2
                countRabbits 4 3 `shouldBe` 2
                countRabbits 5 3 `shouldBe` 3
                countRabbits 6 3 `shouldBe` 4
            it "should work after the sample sequence" $ do
                countRabbits 7 3 `shouldBe` 5
                countRabbits 8 3 `shouldBe` 7
            it "should work with a lower m" $ do
                countRabbits 1 2 `shouldBe` 1
                countRabbits 3 2 `shouldBe` 1
                countRabbits 8 2 `shouldBe` 1
                countRabbits 9 2 `shouldBe` 1
                countRabbits 13 2 `shouldBe` 1
                countRabbits 24 2 `shouldBe` 1
            it "should work with a degenerative sequence" $ do
                countRabbits 1 1 `shouldBe` 1
                countRabbits 3 1 `shouldBe` 0
                countRabbits 5 1 `shouldBe` 0
            it "should work with an higher m" $ do
                countRabbits 1 4 `shouldBe` 1
                countRabbits 2 4 `shouldBe` 1
                countRabbits 3 4 `shouldBe` 2
                countRabbits 4 4 `shouldBe` 3
                countRabbits 5 4 `shouldBe` 4
                countRabbits 6 4 `shouldBe` 6
                countRabbits 7 4 `shouldBe` 9
                countRabbits 8 4 `shouldBe` 13
