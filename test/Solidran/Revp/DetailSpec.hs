module Solidran.Revp.DetailSpec (spec) where

import Test.Hspec
import Solidran.Revp.Detail

spec :: Spec
spec = do
    describe "Solidran.Revp.Detail" $ do
        describe "isReversePalindrome" $ do
            it "should return true on empty string" $ do
                isReversePalindrome ""
                    `shouldBe` True
            it "should work as expected" $ do
                isReversePalindrome "GCATGC"
                    `shouldBe` True
            it "should return false on non reverse palindromes" $ do
                isReversePalindrome "GCATAC" 
                    `shouldBe` False
        describe "findSequencesOfLength" $ do
            it "should return an empty list on empty input" $ do
                findSequencesOfLength 3 "" 
                    `shouldBe` []
                findSequencesOfLength 0 ""
                    `shouldBe` []
            it "should return all 0-length sequences" $ do
                findSequencesOfLength 0 "ABC"
                    `shouldBe` [(1, ""), (2, ""), (3, "")]
            it "should work fine with numbers" $ do
                findSequencesOfLength 2 [1, 2, 3]
                    `shouldBe` [(1, [1, 2]), (2, [2, 3])]
            it "should work fine with strings" $ do
                findSequencesOfLength 3 "ABCDEF"
                    `shouldBe` [(1, "ABC"), (2, "BCD"), (3, "CDE"), (4, "DEF")]
        describe "findReversePalindromes" $ do
            it "should work on the given example" $ do
                findReversePalindromes [4..12] "TCAATGCATGCGGGTCTATATGCAT"
                    `shouldBe` [ (5, 4)
                               , (7, 4)
                               , (17, 4)
                               , (18, 4)
                               , (21, 4)
                               , (4, 6)
                               , (6, 6)
                               , (20, 6) ]
