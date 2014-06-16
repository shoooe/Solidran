module Solidran.FunctionSpec (spec) where

import Test.Hspec
import Solidran.Function

spec :: Spec
spec = do
    describe "Solidran.Function" $ do
        describe "composeN" $ do
            it "should correctly compose 0 times" $ do
                composeN 0 succ 42 `shouldBe` 42
            it "should compose correctly" $ do
                composeN 1 succ 42 `shouldBe` 43
                composeN 2 succ 42 `shouldBe` 44
                composeN 5 succ 42 `shouldBe` 47
