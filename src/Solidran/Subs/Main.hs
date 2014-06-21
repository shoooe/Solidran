module Main where

import Solidran.Output
import Solidran.Subs.Detail

main :: IO ()
main = do
    cont <- getContents
    let [haystack, needle] = lines cont
    putStrLn . output $ findOccs needle haystack 
