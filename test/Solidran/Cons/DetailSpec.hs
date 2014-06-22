module Solidran.Cons.DetailSpec (spec) where

import Test.Hspec
import Solidran.Cons.Detail
import Data.List (transpose)
import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "Solidran.Cons.Detail" $ do
        describe "consensus" $ do
            it "should return an empty string on empty input" $ do
                consensus [] `shouldBe` []
            it "should work on single row" $ do
                consensus ["ACGOTGNOHRGGEDDGHOHUI"]
                    `shouldBe`
                        ['G']
            it "should work on the given example" $ do
                consensus 
                    ( transpose 
                        [ "ATCCAGCT"
                        , "GGGCAACT"
                        , "ATGGATCT"
                        , "AAGCAACC"
                        , "TTGGAACT"
                        , "ATGCCATT"
                        , "ATGGCACT" ] )
                    `shouldBe` "ATGCAACT"
        describe "profileMat" $ do
            it "should work on an empty string" $ do
                profileMat [] 
                    `shouldBe` 
                        Map.fromList 
                            [ ('A', [])
                            , ('C', [])
                            , ('G', [])
                            , ('T', []) ]
            it "should work on the given example" $ do
                profileMat
                    ( transpose 
                        [ "ATCCAGCT"
                        , "GGGCAACT"
                        , "ATGGATCT"
                        , "AAGCAACC"
                        , "TTGGAACT"
                        , "ATGCCATT"
                        , "ATGGCACT" ] )
                    `shouldBe` Map.fromList
                        [ ('A', [5, 1, 0, 0, 5, 5, 0, 0])
                        , ('C', [0, 0, 1, 4, 2, 0, 6, 1])
                        , ('G', [1, 1, 6, 3, 0, 1, 0, 0])
                        , ('T', [1, 5, 0, 0, 0, 1, 1, 6]) ]
