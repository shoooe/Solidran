module Solidran.MathSpec (spec) where

import Test.Hspec
import Solidran.Math

spec :: Spec
spec = do
    describe "Solidran.Math" $ do
        describe "factorial" $ do
            it "should return 1 on 0" $ do
                factorial 0 `shouldBe` 1
            it "should return 1 on 1" $ do
                factorial 1 `shouldBe` 1
            it "should work for other 'n's" $ do
                factorial 2 `shouldBe` 2
                factorial 3 `shouldBe` 6
                factorial 4 `shouldBe` 24
                factorial 5 `shouldBe` 120
