module Solidran.Fasta 
    ( parse
    ) where

import Data.Map (Map)
import Solidran.List (splitBy)
import qualified Data.Map as Map

parseSingle :: String -> (String, String)
parseSingle inp =
    let (l:r) = lines inp
    in  (l, concat r)

parse :: String -> Map String String
parse inp = 
    let bks = filter (/="") . splitBy (=='>') $ inp
    in  Map.fromList (map parseSingle bks)
