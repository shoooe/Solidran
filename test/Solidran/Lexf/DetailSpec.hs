module Solidran.Lexf.DetailSpec (spec) where

import Test.Hspec
import Solidran.Lexf.Detail

spec :: Spec
spec = do
    describe "Solidran.Lexf.Detail" $ do
        describe "getAlphabetStrings" $ do
            it "should return an empty list on empty alphabet" $ do
                getAlphabetStrings 2 []
                    `shouldBe` []
            it "should return a single string on 0-length strings" $ do
                getAlphabetStrings 0 "AB"
                    `shouldBe` [""]
            it "should work for 1-length strings" $ do
                getAlphabetStrings 1 "TAGC"
                    `shouldBe` ["T", "A", "G", "C"]
            it "should work for 1-symbol alphabets" $ do
                getAlphabetStrings 3 "A"
                    `shouldBe` ["AAA"]
            it "should work on the given example" $ do
                getAlphabetStrings 2 "TAGC"
                    `shouldBe` [ "TT", "TA", "TG", "TC"
                               , "AT", "AA", "AG", "AC"
                               , "GT", "GA", "GG", "GC"
                               , "CT", "CA", "CG", "CC" ]
