module Main where

import Solidran.Output (output)
import Solidran.Math (factorial)
import Data.List (permutations)

main :: IO ()
main = do
    n <- getLine
    let rn = read n
    putStrLn . show . factorial $ rn
    mapM_ (putStrLn . output) (permutations [1..rn])
