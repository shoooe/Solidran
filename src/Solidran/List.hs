module Solidran.List
    ( countDistinct
    ) where

import Data.Map (Map)
import qualified Data.Map as Map

countDistinct :: Ord a => [a] -> Map a Int
countDistinct ls = foldr fn Map.empty ls
    where fn e map = Map.insertWith (+) e 1 map
