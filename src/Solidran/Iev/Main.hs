module Main where

import Solidran.Iev.Detail

main :: IO ()
main = do
    i <- getLine
    let [a, b, c, d, e, f] = map (read :: String -> Int) . words $ i
    print . expectedDom $ (a, b, c, d, e, f)
