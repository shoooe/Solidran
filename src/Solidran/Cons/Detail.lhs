<h2>Consensus and Profile</h2>

This problem can be found [here](http://rosalind.info/problems/cons/).

<h3>Exports</h3>

> module Solidran.Cons.Detail 
>     ( readInput
>     , consensus
>     , profileMat
>     ) where

<h3>Imports</h3>

> import Data.Map (Map)
> import Solidran.List (countIf, countDistinct)
> import Data.Function (on)
> import Data.List (maximumBy, transpose)
> import qualified Data.Map as Map
> import qualified Solidran.Fasta as Fasta

<h3>Read input</h3>

Given the considerations above we need to:

 - parse a FASTA format
 - drop the keys
 - transpose the resulting matrix

> readInput :: String -> [[Char]]
> readInput = transpose . Map.elems . Fasta.parse

<h3>Profile matrix</h3>

We can see the profile matrix as a `Map Char [Int]`, which maps one of the letters (A, C, G, T) into a list of its occurrences over all the columns:

> type ProfileMat = Map Char [Int]

To generate the profile matrix we can fold our matrix:

> profileMat :: [[Char]] -> ProfileMat
> profileMat = foldr updateMap iniMap 
>     where iniMap = Map.fromList 
>               [ ('A', [])
>               , ('C', [])
>               , ('G', [])
>               , ('T', []) ]
>           updateMap s c     = Map.mapWithKey (countChar s) c
>           countChar s c l   = countIf (==c) s : l

<h3>Consensus</h3>

A consensus is just a list of characters:

> type Consensus  = [Char]

and to calculate it we can fold our matrix with the most frequent letter:

> consensus :: [[Char]] -> Consensus
> consensus = map (fst . maximumBy (compare `on` snd) . charMap)
>     where charMap = Map.toList . countDistinct
