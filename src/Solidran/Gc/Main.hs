module Main where

import qualified Solidran.Fasta as Fasta
import Solidran.Gc.Detail 

main :: IO ()
main = do
    c <- getContents
    let (l, v) = highestContent "GC" . Fasta.parse $ c
    putStrLn l
    print v
