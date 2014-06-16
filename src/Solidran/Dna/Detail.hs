module Solidran.Dna.Detail where

import Data.Map (Map)
import Solidran.List (countDistinct)
import qualified Data.Map as Map

nucleoCount :: String -> [Int]
nucleoCount string =
    let map = countDistinct string
    in      [ Map.findWithDefault 0 'A' map
            , Map.findWithDefault 0 'C' map 
            , Map.findWithDefault 0 'G' map 
            , Map.findWithDefault 0 'T' map ]
