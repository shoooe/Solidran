module Solidran.Gc.DetailSpec (spec) where

import Test.Hspec
import Solidran.Gc.Detail
import qualified Solidran.Fasta as Fasta
import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "Solidran.Gc.Detail" $ do
        desribe "highestContent" $ do
            it "should work on the sample" $ do
                highestContent "GC" . Fasta.parse $
                    unlines
                        [ "Rosalind_6404"
                        , "CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC"
                        , "TCCCACTAATAATTCTGAGG"
                        , ">Rosalind_5959"
                        , "CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT"
                        , "ATATCCATTTGTCAGCAGACACGC"
                        , ">Rosalind_0808"
                        , "CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC"
                        , "TGGGAACCTGCGGGCAGTAGGTGGAAT" ]
                `shouldBe`
                    ("Rosalind_0808", 60.91954022988506)
            it "should work on other strings" $ do
                highestContent "GC" . Fasta.parse $
                    unlines 
                        [ ">abc"
                        , "CCGA"
                        , "AAGA"
                        , ">def"
                        , "ALOE"
                        , "CIGE" ]
                `shouldBe`
                    ("abc", 50.0)
            it "should work with values on a single line" $ do
                highestContent "GC" . Fasta.parse $
                    unlines 
                        [ ">abc"
                        , "ACCGE"
                        , ">def"
                        , "GGCGC"
                        , ">ghi"
                        , "AREAO" ]
                `shouldBe`
                    ("def", 100)
            it "should work with any letter" $ do
                highestContent "A" . Fasta.parse $
                    unlines 
                        [ ">abc"
                        , "ACCGEEEF"
                        , ">def"
                        , "GAAA"
                        , ">ghi"
                        , "ARAO" ]
                `shouldBe`
                    ("def", 75.0)  
            it "should work with more than two letters" $ do
                highestContent "AGI" . Fasta.parse $
                    unlines 
                        [ ">abc"
                        , "ACCGEEEF"
                        , ">def"
                        , "GAAA"
                        , ">ghi"
                        , "AO" ]
                `shouldBe`
                    ("def", 100.0) 
            it "should work with empty strings" $ do
                highestContent "ABCDEF" . Fasta.parse $
                    unlines 
                        [ ">abc"
                        , ""
                        , ">def"
                        , "FEC" ]
                `shouldBe`
                    ("def", 100.0)
