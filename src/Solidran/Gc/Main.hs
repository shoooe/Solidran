module Main where

import Solidran.Gc.Detail 

main :: IO ()
main = do
    c <- getContents
    let (l, v) = highestContent "GC" c
    putStrLn l
    print v
