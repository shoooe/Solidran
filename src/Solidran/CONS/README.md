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

 - separate each line
 - `splitBy` the lines that begins with a `>`
 - concatenate the groups returned by `splitBy`

```haskell
readInput :: String -> [String]
readInput = filter (/="") . map concat . splitBy ((=='>') . head) . lines
```

####Profile matrix

We can see the profile matrix as a `Map Char [Int]`, which maps one of the letters (A, C, G, T) into a list of its occurrences over all the columns:

```haskell
type ProfileMat = Map Char [Int]
```

To generate the profile matrix we can fold our matrix with the above matrix as the counter:

```haskell
profileMat :: [String] -> ProfileMat
profileMat = foldRCol fn iniMap 
    where iniMap = Map.fromList 
              [ ('A', [])
              , ('C', [])
              , ('G', [])
              , ('T', []) ]
          fn col = Map.mapWithKey ((:) . countElem col)
```

####Consensus

A consensus is just a string:

```haskell
type Consensus  = [Char]
```

and to calculate it we can fold our matrix with the `mostFrequent` of our set of 4 characters:

```haskell
consensus :: [String] -> Consensus
consensus = foldRCol ((:) . mostFrequent ['A', 'C', 'G', 'T']) ""
```

####Output

A list of ints is shown as space-separated:

```haskell
showList :: [Int] -> String
showList l = concat . intersperse " " . map show $ l
```

and a map is shown as a bunch of `key: value` lines:

```haskell
showMap :: Map Char [Int] -> String
showMap = Map.foldrWithKey fn ""
    where fn k v c = concat [[k], ": ", showList v, "\n", c]
```
