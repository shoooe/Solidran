module Solidran.Dna.Detail where

import Data.Map (Map)
import Solidran.List (countDistinct)
import qualified Data.Map as Map

nucleoCount :: String -> [Int]
nucleoCount string =
    let mp = countDistinct string
    in      [ Map.findWithDefault 0 'A' mp
            , Map.findWithDefault 0 'C' mp 
            , Map.findWithDefault 0 'G' mp 
            , Map.findWithDefault 0 'T' mp ]
