module Solidran.OutputSpec (spec) where

import Test.Hspec
import Solidran.Output
import Data.Map (Map)
import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "Solidran.Output" $ do
        describe "output" $ do
            it "should display a char" $ do
                output 'a' `shouldBe` "a"
            it "should display a string" $ do
                output "" `shouldBe` ""
                output "abc" `shouldBe` "a b c"
            it "should diplay a list" $ do
                output ([] :: [Int]) `shouldBe` ""
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
            it "should display a map" $ do
                output (( Map.fromList 
                        [ ('a', 123)
                        , ('b', 752)] ) :: Map Char Int )
                    `shouldBe` "a: 123\nb: 752\n"
            it "should display a string as key in a map" $ do
                output (( Map.fromList 
                        [ ("abc", [1, 5, 3])
                        , ("def", [5])
                        , ("ghi", []) ] ) :: Map String [Int] )
                    `shouldBe` "abc: 1 5 3\ndef: 5\nghi: \n"
