module Solidran.List
    ( splitBy
    , countDistinct
    , countIf
    , groupEvery
    ) where

import Data.Map (Map)
import qualified Data.Map as Map

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy c ls = 
    let (l, r) = span (not . c) ls
    in l : (splitBy c . drop 1 $ r)

countDistinct :: Ord a => [a] -> Map a Int
countDistinct ls = foldr fn Map.empty ls
    where fn e map = Map.insertWith (+) e 1 map

countIf :: (a -> Bool) -> [a] -> Int
countIf _ [] = 0
countIf p (x:xs) = c + countIf p xs
    where c = if p x then 1 else 0

groupEvery :: Int -> [a] -> [[a]]
groupEvery 0 _ = []
groupEvery _ [] = []
groupEvery e l
    | length l > e  = (take e l) : (groupEvery e (drop e l))
    | otherwise     = [l]
