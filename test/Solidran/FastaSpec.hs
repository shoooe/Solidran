module Solidran.FastaSpec (spec) where

import Test.Hspec
import Solidran.Fasta
import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "Solidran.Fasta" $ do
        describe "parse" $ do
            it "should give an empty map on empty string" $ do
                parse "" `shouldBe` Map.empty
            it "should parse a single record" $ do
                parse $ unlines
                    [ ">label1"
                    , "content1" ]
                `shouldBe`
                    Map.fromList [("label1", "content1")]
            it "should parse multiple records" $ do
                parse $ unlines
                    [ ">label1"
                    , "content1"
                    , ">label2"
                    , "content2" ]
                `shouldBe`
                    Map.fromList 
                        [ ("label1", "content1")
                        , ("label2", "content2") ]
            it "should parse empty contents" $ do
                parse $ unlines
                    [ ">label1"
                    , ">label2" ]
                `shouldBe`
                    Map.fromList
                        [ ("label1", "")
                        , ("label2", "") ]
            it "should parse empty and non-empty" $ do
                parse $ unlines
                    [ ">label1"
                    , "content1"
                    , ">label2" ]
                `shouldBe`
                    Map.fromList
                        [ ("label1", "content1")
                        , ("label2", "") ]
