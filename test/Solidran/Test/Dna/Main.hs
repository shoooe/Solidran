module Main where

import Solidran.Dna.Counter (Counter(..))
import qualified Solidran.Dna.Counter as Counter
import Test.Hspec (shouldBe, hspec, describe, it)

main :: IO ()
main = hspec $ do
    describe "Solidran.Dna" $ do
        describe "Counter" $ do
            it "should count on an empty string" $ do
                Counter.count "" `shouldBe` Counter 0 0 0 0
            it "should count correctly" $ do
                Counter.count "AGCT" `shouldBe` Counter 1 1 1 1
                Counter.count "AAAA" `shouldBe` Counter 4 0 0 0
                Counter.count "AGGT" `shouldBe` Counter 1 0 2 1
            it "shouldn't count lower case letters" $ do
                Counter.count "aaAa" `shouldBe` Counter 1 0 0 0
                Counter.count "agct" `shouldBe` Counter 0 0 0 0
                Counter.count "aGtTCaAA" `shouldBe` Counter 2 1 1 1
            it "should ignore other letters" $ do
                Counter.count "TUaoA" `shouldBe` Counter 1 0 0 1
