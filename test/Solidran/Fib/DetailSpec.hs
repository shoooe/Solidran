module Solidran.Fib.DetailSpec (spec) where

import Test.Hspec
import Solidran.Fib.Detail

spec :: Spec
spec = do
    describe "Solidran.Fib.Detail" $ do
        describe "nextPair" $ do
            it "should correctly calculate the next sequence" $ do
                nextPair 1 (0, 1) `shouldBe` (1, 1)
                nextPair 1 (1, 1) `shouldBe` (1, 2)
                nextPair 1 (1, 2) `shouldBe` (2, 3)
                nextPair 1 (2, 3) `shouldBe` (3, 5)
                nextPair 1 (3, 5) `shouldBe` (5, 8)
        describe "rabbitsCount" $ do
            it "should correctly calculate the rabbits count" $ do
                rabbitsCount 5 2 `shouldBe` 11
                rabbitsCount 5 3 `shouldBe` 19
