module Solidran.Gc.Detail 
    ( highestContent
    ) where

import Solidran.List (countIf)
import Data.List (maximumBy)
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map

calcPercent :: [Char] -> String -> Double
calcPercent ls str =
    if tot > 0 
        then fromIntegral is * 100 / fromIntegral tot
        else 0.0
    where is  = countIf (`elem` ls) str
          tot = length str

highestContent :: [Char] -> Map String String -> (String, Double)
highestContent ls mp =
    maximumBy (compare `on` snd) list
        where list = Map.toList . Map.map (calcPercent ls) $ mp
