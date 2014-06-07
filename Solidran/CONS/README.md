###[Consensus and Profile](http://rosalind.info/problems/cons/)

####Don't assume

I've wrongly assumed that the give example dataset:

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
    >label1
    DNA string 1 part 1
    DNA string 1 part 2
    ...
    >label
    DNA string 2 part 1
    DNA string 2 part 2
    ...
...
```

that is: the DNA string can be split between lines.

####Our beloved `splitBy`

First of all let's take our `splitBy` missing function that takes a list of elements and splits them by a predicate. The behavior should be:

```haskell
splitBy ',' "1,2,3,4"
-- ['1', '2', '3', '4']

splitBy ',' ""
-- []
```

and here's the implementation:

```haskell
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy c ls = 
    let (l, r) = span (not . c) ls
    in l : (splitBy c . drop 1 $ r)
```

that uses `span` to divide each segments and just remembers to `drop 1` element at the beginning (the second element of the returned tuple still contains the element that satisfied the predicate).

####Read input

Given the considerations above we need to:

 - separate each line
 - `splitBy` the lines that begins with a `>`
 - concatenate the groups returned by `splitBy`

```haskell
readInput :: String -> [String]
readInput = filter (/="") . map concat . splitBy ((=='>') . head) . lines
```

####Utility functions

Let's define a function that folds on a matrix (seen as a list of list) on the right, by its columns:

```haskell
foldRCol :: ([a] -> b -> b) -> b -> [[a]] -> b
foldRCol fn c l =
    if null heads 
        then c
        else heads `fn` foldRCol fn c tails
    where heads = concat . map (take 1) $ l
          tails = map (drop 1) l
```

, a function that counts the occurrences of an element in a list:

```haskell
countElem :: Eq a => [a] -> a -> Int
countElem ls e = length $ filter (== e) ls
```

and finally a function that, given a set of elements, returns the one that appears most often:

```haskell
mostFrequent :: Eq a => [a] -> [a] -> a
mostFrequent sm ls = fst $ maximumBy (compare `on` snd) $ map fn sm
    where fn e = (e, length $ filter (==e) ls)
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
