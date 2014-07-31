module Solidran.Mrna.DetailSpec (spec) where

import Test.Hspec
import Solidran.Mrna.Detail
import Control.Monad (mapM_)
import Data.List (permutations)

spec :: Spec
spec = do
    describe "Solidran.Mrna.Detail" $ do
        describe "rnaCombsMod" $ do
            it "should work on the given example" $ do
                rnaCombsMod 1000000 "MA" `shouldBe` 12
            it "should work on an empty string" $ do
                rnaCombsMod 1000000 "" `shouldBe` 3
            it "should correctly take the modulo" $ do
                rnaCombsMod 10 "MA" `shouldBe` 2
            it "should work with other examples" $ do
                rnaCombsMod 100000 "SLIDRAN" `shouldBe` 31104
                rnaCombsMod 100 "SLIDRAN" `shouldBe` 4
            it "should be the same for different permutations" $ do
                let alphabet = "ILRSS"
                let perms = permutations alphabet
                let pairs = zip perms perms
                mapM_ (\(a, b) -> do
                    rnaCombsMod 145 a `shouldBe` rnaCombsMod 145 b)
                        pairs
            it "should work on long strings" $ do
                rnaCombsMod 42 (concat
                    [ "FHFHWIFKHSFRFEGWKKAYTGYYSHWIYREQTYPMSNQMQMPMISAMVCTCFG"
                    , "FHFHWIFKHSFRFEGWKKAYTGYYSHWIYREQTYPMSNQMQMPMISAMVCTCFG"
                    , "FHFHWIFKHSFRFEGWKKAYTGYYSHWIYREQTYPMSNQMQMPMISAMVCTCFG" ])
                    `shouldBe` 18
