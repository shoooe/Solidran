module Solidran.OutputSpec (spec) where

import Test.Hspec
import Solidran.Output

spec :: Spec
spec = do
    describe "Solidran.Output" $ do
        it "should display a char" $ do
            output 'a' `shouldBe` "a"
        it "should display a string" $ do
            output "" `shouldBe` ""
            output "abc" `shouldBe` "a b c"
        it "should diplay a list" $ do
            output ([1, 2, 3] :: [Int]) `shouldBe` "1 2 3"
            output (['a', 'b'] :: [Char]) `shouldBe` "a b"
            output ([2.0, 3.0] :: [Double]) `shouldBe` "2.0 3.0"
        it "should diplay an int" $ do
            output (0 :: Int) `shouldBe` "0"
            output (1 :: Int) `shouldBe` "1"
            output (198 :: Int) `shouldBe` "198"
        it "should display an integer" $ do
            output (12937652894990794718471978 :: Integer) 
                `shouldBe` 
                    "12937652894990794718471978"
