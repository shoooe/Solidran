module Main where

import Solidran.Output (output, Str(..))
import Solidran.Grph.Detail

outputResult :: [(String, String)] -> String
outputResult = unlines . map (\ (a, b) -> output (Str a, Str b))

main :: IO ()
main = getContents >>= (putStr . outputResult . adjacencyList 3)
