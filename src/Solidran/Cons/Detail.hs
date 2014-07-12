module Solidran.Cons.Detail 
    ( readInput
    , consensus
    , profileMat
    ) where

import Data.Map (Map)
import Solidran.List (countIf, countDistinct)
import Data.Function (on)
import Data.List (maximumBy, transpose)
import qualified Data.Map as Map
import qualified Solidran.Fasta as Fasta

type ProfileMat = Map Char [Int]
type Consensus  = [Char]

readInput :: String -> [[Char]]
readInput = transpose . Map.elems . Fasta.parse

consensus :: [[Char]] -> Consensus
consensus = map (fst . maximumBy (compare `on` snd) . charMap)
    where charMap = Map.toList . countDistinct

profileMat :: [[Char]] -> ProfileMat
profileMat = foldr updateMap iniMap 
    where iniMap = Map.fromList 
              [ ('A', [])
              , ('C', [])
              , ('G', [])
              , ('T', []) ]
          updateMap s c     = Map.mapWithKey (countChar s) c
          countChar s c l   = countIf (==c) s : l
