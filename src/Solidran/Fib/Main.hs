module Main where

import Solidran.Fib.Detail (rabbitsCount)

main :: IO ()
main = do
    ln <- getLine
    let [n, k] = map read $ words ln
    print $ rabbitsCount n k
