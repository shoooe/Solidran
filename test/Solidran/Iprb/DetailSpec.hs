module Solidran.Iprb.DetailSpec (spec) where

import Test.Hspec
import Solidran.Iprb.Detail
import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "Solidran.Iprb.Detail" $ do
        describe "totalProb" $ do
            it "should work on the given example" $ do
                totalProb (Map.fromList [(HomoDom, 2), (Hetero, 2), (HomoRec, 2)])
                    `shouldBe` 0.7833333333333333
            it "should return 0 on no organisms" $ do
                totalProb (Map.fromList [])
                    `shouldBe` 0.0     
            it "should return 1 when there are all HomoDom" $ do
                totalProb (Map.fromList [(HomoDom, 5)])
                    `shouldBe` 1.0
            it "should return 0 on all HomoRec" $ do
                totalProb (Map.fromList [(HomoRec, 52)])
                    `shouldBe` 0.0
            it "should return 3/4 on all Hetero" $ do
                totalProb (Map.fromList [(Hetero, 43)])
                    `shouldBe` 0.75
            it "should return 0 on single organisms" $ do
                totalProb (Map.fromList [(HomoDom, 1)]) 
                    `shouldBe` 0.0
                totalProb (Map.fromList [(Hetero, 1)]) 
                    `shouldBe` 0.0
                totalProb (Map.fromList [(HomoRec, 1)]) 
                    `shouldBe` 0.0
