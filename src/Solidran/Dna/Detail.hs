module Solidran.Dna.Detail where

import Solidran.List (countDistinct)
import qualified Data.Map as Map

nucleoCount :: String -> [Int]
nucleoCount string =
    let m = countDistinct string
    in      [ Map.findWithDefault 0 'A' m
            , Map.findWithDefault 0 'C' m
            , Map.findWithDefault 0 'G' m
            , Map.findWithDefault 0 'T' m ]
