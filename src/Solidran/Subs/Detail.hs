module Solidran.Subs.Detail where

import Data.List (isPrefixOf)

findOccs :: Eq a => [a] -> [a] -> [Int]
findOccs [] _ = []
findOccs _ [] = []
findOccs n h = filter fn ixs
    where fn i  = isPrefixOf n . drop (i-1) $ h
          ixs   = [1..((length h - length n) + 1)]
