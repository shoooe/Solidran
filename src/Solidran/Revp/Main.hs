module Main where

import Solidran.Revp.Detail
import Solidran.Output

main :: IO ()
main = do
    c <- getContents
    let s = concat . filter (\(x:_) -> x /= '>') . lines $ c 
    mapM_ (putStrLn . output) . findReversePalindromes [4..12] $ s
