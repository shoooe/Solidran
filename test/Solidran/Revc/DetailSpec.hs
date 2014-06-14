module Solidran.Revc.DetailSpec (spec) where

import Solidran.Revc.Detail
import Test.Hspec

spec :: Spec
spec = do
    describe "Solidran.Revc.Detail" $ do
        it "should do nothing with an empty string" $ do
            complementDna "" `shouldBe` ""
        it "should correctly complement a dna string" $ do
            complementDna "A" `shouldBe` "T"
            complementDna "AAAACCCGGT"
                `shouldBe`
                    "ACCGGGTTTT"
