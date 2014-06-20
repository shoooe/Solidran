module Solidran.Prot.DetailSpec (spec) where

import Test.Hspec
import Solidran.Prot.Detail

spec :: Spec
spec = do
    describe "Solidran.Prot.Detail" $ do
        describe "encode" $ do
            it "should work on the example" $ do
                encode "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"
                    `shouldBe`
                        "MAMAPRTEINSTRING\0"
            it "should return empty string on empty string" $ do
                encode "" `shouldBe` ""
            it "should work on a null terminated codon" $ do
                encode "UAA" `shouldBe` "\0"
            it "should work on two null terminated codons" $ do
                encode "UAAUAG" `shouldBe` "\0\0"
                encode "CUUUAACCCUAACCA" `shouldBe` "L\0P\0P"
            it "should work on other examples" $ do
                encode "CUUUUCUCAGCU" `shouldBe` "LFSA"
                encode "CCACCACCACCA" `shouldBe` "PPPP"
                encode "UCGCCCAGAACC" `shouldBe` "SPRT"
