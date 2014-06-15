module Solidran.ListSpec (spec) where

import Test.Hspec
import Solidran.List
import Data.Map (Map)
import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "Solidran.Dna.List" $ do
        describe "splitBy" $ do
            it "should do nothing on an empty list" $ do
                splitBy (==',') "" `shouldBe` []
            it "should split correctly" $ do
                splitBy (==',') "a,bc,erg,s,wer,ss"
                    `shouldBe`
                        ["a", "bc", "erg", "s", "wer", "ss"]
            it "should split also list of numbers" $ do
                splitBy (==0) [1, 0, 1, 1, 3, 0, 4, 2]
                    `shouldBe`
                        [[1], [1, 1, 3], [4, 2]]
        describe "countDistinct" $ do
            it "should return an empty map on empty string" $ do
                countDistinct "" `shouldBe` Map.empty
            it "should count all letters" $ do
                countDistinct "ajuu92333" 
                    `shouldBe`
                        Map.fromList [ ('a', 1)
                                     , ('j', 1)
                                     , ('u', 2)
                                     , ('9', 1)
                                     , ('2', 1)
                                     , ('3', 3) ]
        describe "countIf" $ do
            it "should return 0 on empty string" $ do
                countIf (=='A') "" `shouldBe` 0
            it "should count the number of elements that satisfy the predicate" $ do
                countIf (=='C') "AYCGRIONCXCC" `shouldBe` 4
