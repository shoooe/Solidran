module Main where

import Solidran.Lexf.Detail

main :: IO ()
main = do
    alphabet    <- (getLine >>= (return . filter (/= ' ')))
    n           <- (getLine >>= (return . read))
    mapM_ (putStrLn) $ getAlphabetStrings n alphabet
