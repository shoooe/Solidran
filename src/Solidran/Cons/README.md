###[Consensus and Profile](http://rosalind.info/problems/cons/)

####Don't assume

I've wrongly assumed that the given example dataset:

```
>label 1
DNA string 1
>label 2
DNA string 2
...
```

would mean that we would receive 20 lines for 10 DNA strings (a pair of label and line for each).

The real dataset format is in the form:

```
    >label 1
    DNA string 1 part 1
    DNA string 1 part 2
    ...
    >label 2
    DNA string 2 part 1
    DNA string 2 part 2
    ...
...
```

that is: the DNA string can be split between lines.

####Read input

Given the considerations above we need to:

 - parse a FASTA format
 - drop the keys
 - transpose the resulting matrix

```haskell
readInput :: String -> [[Char]]
readInput = transpose . Map.elems . Fasta.parse
```

####Profile matrix

We can see the profile matrix as a `Map Char [Int]`, which maps one of the letters (A, C, G, T) into a list of its occurrences over all the columns:

```haskell
type ProfileMat = Map Char [Int]
```

To generate the profile matrix we can fold our matrix:

```haskell
profileMat :: [[Char]] -> ProfileMat
profileMat = foldr updateMap iniMap 
    where iniMap = Map.fromList 
              [ ('A', [])
              , ('C', [])
              , ('G', [])
              , ('T', []) ]
          updateMap s c     = Map.mapWithKey (countChar s) c
          countChar s c l   = countIf (==c) s : l
```

####Consensus

A consensus is just a list of characters:

```haskell
type Consensus  = [Char]
```

and to calculate it we can fold our matrix with the most frequent letter:

```haskell
consensus :: [[Char]] -> Consensus
consensus = map (fst . maximumBy (compare `on` snd) . charMap)
    where charMap = Map.toList . countDistinct
```
