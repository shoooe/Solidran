module Solidran.Grph.DetailSpec (spec) where

import Test.Hspec
import Solidran.Grph.Detail

spec :: Spec
spec = do
    describe "Solidran.Grph.Detail" $ do
        describe "adjacencyList" $ do
            it "should work on the given example" $ do
                adjacencyList 3
                    (unlines 
                        [ ">Rosalind_0498"
                        , "AAATAAA"
                        , ">Rosalind_2391"
                        , "AAATTTT"
                        , ">Rosalind_2323"
                        , "TTTTCCC"
                        , ">Rosalind_0442"
                        , "AAATCCC"
                        , ">Rosalind_5013"
                        , "GGGTGGG" ] )
                    `shouldBe`
                        [ ("Rosalind_2391", "Rosalind_2323")
                        , ("Rosalind_0498", "Rosalind_0442")
                        , ("Rosalind_0498", "Rosalind_2391") ]
            it "should return nothing on empty input" $ do
                adjacencyList 3 "" `shouldBe` []
            it "should work on the sample input with O2" $ do
                adjacencyList 2
                    (unlines 
                        [ ">Rosalind_0498"
                        , "AAATAAA"
                        , ">Rosalind_2391"
                        , "AAATTTT"
                        , ">Rosalind_2323"
                        , "TTTTCCC"
                        , ">Rosalind_0442"
                        , "AAATCCC"
                        , ">Rosalind_5013"
                        , "GGGTGGG" ] )
                    `shouldBe`
                        [ ("Rosalind_2391", "Rosalind_2323")
                        , ("Rosalind_0498", "Rosalind_0442")
                        , ("Rosalind_0498", "Rosalind_2391") ] 
            it "two single-letter DNA strings should always be adjacent" $ do
                let dna = ">a\nAAAAAAA\n>b\nAAAAAAA\n"
                    rst = [("b", "a"), ("a", "b")] 
                adjacencyList 1 dna `shouldBe` rst
                adjacencyList 2 dna `shouldBe` rst
                adjacencyList 3 dna `shouldBe` rst
                adjacencyList 4 dna `shouldBe` rst
                adjacencyList 5 dna `shouldBe` rst
                adjacencyList 6 dna `shouldBe` rst
                adjacencyList 7 dna `shouldBe` rst
            it "should return an empty string if the two nodes are not adjacent" $ do
                adjacencyList 1 ">a\nAAAAAAA\n>b\nXAAAAAC\n"
                    `shouldBe` []
