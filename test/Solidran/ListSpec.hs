module Solidran.ListSpec (spec) where

import Test.Hspec
import Solidran.List (countDistinct)
import Data.Map (Map)
import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "Solidran.Dna.List" $ do
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
