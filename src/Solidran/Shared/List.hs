module Solidran.Shared.List 
    ( countDistinct
    , splitBy
    , countElem
    , countFrequency
    , rightFoldCol ) 
    where

import Data.List (span)
import Data.Map (Map)
import qualified Data.Map as Map

countDistinct :: Ord a => [a] -> Map a Int
countDistinct ls = foldr fn Map.empty ls
    where fn e map = Map.insertWith (+) e 1 map

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy c ls = 
    let (l, r) = span (not . c) ls
    in l : (splitBy c . drop 1 $ r)

countElem :: Eq a => a -> [a] -> Int
countElem e = length . filter (== e)

countFrequency :: Eq a => [a] -> [a] -> [(a, Int)]
countFrequency sm ls = map fn sm
    where fn e = (e, countElem e ls)

rightFoldCol :: ([a] -> b -> b) -> b -> [[a]] -> b
rightFoldCol fn c l =
    if null heads 
        then c
        else heads `fn` rightFoldCol fn c tails
    where heads = concat . map (take 1) $ l
          tails = map (drop 1) l
