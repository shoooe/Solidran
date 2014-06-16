module Solidran.Dna.DetailSpec (spec) where

import Solidran.Dna.Detail
import Test.Hspec

spec :: Spec
spec = do
    describe "Solidran.Dna.Detail" $ do
        describe "nucleoCount" $ do
            it "should count on an empty string" $ do
                nucleoCount "" `shouldBe` [0, 0, 0, 0]
            it "should count correctly" $ do
                nucleoCount "AGCT" `shouldBe` [1, 1, 1, 1]
                nucleoCount "AAAA" `shouldBe` [4, 0, 0, 0]
                nucleoCount "AGGT" `shouldBe` [1, 0, 2, 1]
            it "shouldn't count lower case letters" $ do
                nucleoCount "aaAa" `shouldBe` [1, 0, 0, 0]
                nucleoCount "agct" `shouldBe` [0, 0, 0, 0]
                nucleoCount "aGtTCaAA" `shouldBe` [2, 1, 1, 1]
            it "should ignore other letters" $ do
                nucleoCount "TUaoA" `shouldBe` [1, 0, 0, 1]
