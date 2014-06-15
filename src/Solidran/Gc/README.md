###[Computing GC Content](http://rosalind.info/problems/gc/)

####Parse input

To parse the input we are going to use our FASTA parser located in `Solidran.Fasta`.

####Calculating the percentage 

The percentage of a single record DNA-string is given by:

```haskell
calcPercent :: [Char] -> String -> Double
calcPercent ls str =
    if tot > 0 
        then fromIntegral is * 100 / fromIntegral tot
        else 0.0
    where is  = countIf (`elem` ls) str
          tot = length str
```

If the string is empty we are going to result `0%`; otherwise the percentage is given by the number of elements that *are* in the list (in our case `['C', 'G']`) divided by the total length of the string.

####Highest percentage

The highest percentage is given by:

```haskell
highestContent :: [Char] -> String -> (String, Double)
highestContent ls str =
    maximumBy (compare `on` snd) list
        where list = Map.toList . Map.map (calcPercent ls) $ (Fasta.parse str)
```

where `ls` is the list of characters we are considering (in our case `['C', 'G']`). This is given by mapping the percent calculator (`calcPercent`) on every value of the map and then converting the resulting map back to a list in order to apply `maximumBy` on it.
