module Solidran.Rna.DetailSpec where

import Test.Hspec
import Solidran.Rna.Detail (dnaToRna)

spec :: Spec
spec = do
    describe "Solidran.Dna.Detail" $ do
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
