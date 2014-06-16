module Solidran.Rna.DetailSpec where

import Test.Hspec
import Solidran.Rna.Detail

spec :: Spec
spec = do
    describe "Solidran.Dna.Detail" $ do
        describe "dnaToRna" $ do
            it "should correctly translate DNA to RNA" $ do
                dnaToRna "GATGGAACTTGACTACGTAAATT" 
                    `shouldBe`
                        "GAUGGAACUUGACUACGUAAAUU"
                dnaToRna ""
                    `shouldBe`
                        ""
                dnaToRna "efTTwehufihAUYUGAAAGYATTT"
                    `shouldBe`
                        "efUUwehufihAUYUGAAAGYAUUU" 
