module Main where

import Solidran.Output
import Solidran.Cons.Detail

main :: IO ()
main = do
    c <- getContents
    let ls = readInput c
    putStrLn . consensus $ ls
    putStrLn . output . profileMat $ ls
