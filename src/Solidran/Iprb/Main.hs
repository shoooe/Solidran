module Main where

import Solidran.Iprb.Detail
import qualified Data.Map as Map

main :: IO ()
main = do
    ln <- getLine
    let [k, m, n] = map read . words $ ln
    let tp = totalProb $ Map.fromList 
                 [ (HomoDom, k)
                 , (Hetero,  m)
                 , (HomoRec, n) ]
    print tp
