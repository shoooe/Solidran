###[Translating RNA into Protein](http://rosalind.info/problems/prot/)

####Generating the Map

Probably the most difficult part is taking the string given:

```haskell
str = "UUU F      CUU L      AUU I      GUU V \
    \UUC F      CUC L      AUC I      GUC V \
    \UUA L      CUA L      AUA I      GUA V \
    \UUG L      CUG L      AUG M      GUG V \
    \UCU S      CCU P      ACU T      GCU A \
    \UCC S      CCC P      ACC T      GCC A \
    \UCA S      CCA P      ACA T      GCA A \
    \UCG S      CCG P      ACG T      GCG A \
    \UAU Y      CAU H      AAU N      GAU D \
    \UAC Y      CAC H      AAC N      GAC D \
    \UAA Stop   CAA Q      AAA K      GAA E \
    \UAG Stop   CAG Q      AAG K      GAG E \
    \UGU C      CGU R      AGU S      GGU G \
    \UGC C      CGC R      AGC S      GGC G \
    \UGA Stop   CGA R      AGA R      GGA G \
    \UGG W      CGG R      AGG R      GGG G"
```

and convert it to a `[(a, b)]`, where `a` is the three letter string and `b` is the substitution letter (with `Stop == \0`.

First we define a group by two function that "zips" a list into group of sequential two elements:

```haskell
groupByTwo :: [a] -> [(a, a)]
groupByTwo [] = []
groupByTwo l = [(head l, head . tail $ l)] ++ groupByTwo (tail . tail $ l)
```

Then we define a function to show a single tuple:

```haskell
showTuple :: (String, String) -> String
showTuple (a, "Stop") = concat ["\t, (\"", a, "\", '", "\\0", "')\n"] 
showTuple (a, b) = concat ["\t, (\"", a, "\", '", b, "')\n"]
```

Not exactly the prettiest function, but gets the job done and it's a disposable solution. Not we can just run:

```haskell
mapM_ (putStr . showTuple) str
```
and that will lead us to a 98% of the output. We just need to adjust the beginning and end of the list.

The resulting map is:

```haskell
codonTable :: Map String Char
codonTable =
    Map.fromList
        [ ("UUU", 'F')
        , ("CUU", 'L')
        , ("AUU", 'I')
        , ("GUU", 'V')
        , ("UUC", 'F')
        , ("CUC", 'L')
        , ("AUC", 'I')
        , ("GUC", 'V')
        , ("UUA", 'L')
        , ("CUA", 'L')
        , ("AUA", 'I')
        , ("GUA", 'V')
        , ("UUG", 'L')
        , ("CUG", 'L')
        , ("AUG", 'M')
        , ("GUG", 'V')
        , ("UCU", 'S')
        , ("CCU", 'P')
        , ("ACU", 'T')
        , ("GCU", 'A')
        , ("UCC", 'S')
        , ("CCC", 'P')
        , ("ACC", 'T')
        , ("GCC", 'A')
        , ("UCA", 'S')
        , ("CCA", 'P')
        , ("ACA", 'T')
        , ("GCA", 'A')
        , ("UCG", 'S')
        , ("CCG", 'P')
        , ("ACG", 'T')
        , ("GCG", 'A')
        , ("UAU", 'Y')
        , ("CAU", 'H')
        , ("AAU", 'N')
        , ("GAU", 'D')
        , ("UAC", 'Y')
        , ("CAC", 'H')
        , ("AAC", 'N')
        , ("GAC", 'D')
        , ("UAA", '\0')
        , ("CAA", 'Q')
        , ("AAA", 'K')
        , ("GAA", 'E')
        , ("UAG", '\0')
        , ("CAG", 'Q')
        , ("AAG", 'K')
        , ("GAG", 'E')
        , ("UGU", 'C')
        , ("CGU", 'R')
        , ("AGU", 'S')
        , ("GGU", 'G')
        , ("UGC", 'C')
        , ("CGC", 'R')
        , ("AGC", 'S')
        , ("GGC", 'G')
        , ("UGA", '\0')
        , ("CGA", 'R')
        , ("AGA", 'R')
        , ("GGA", 'G')
        , ("UGG", 'W')
        , ("CGG", 'R')
        , ("AGG", 'R')
        , ("GGG", 'G') ]
```

####Missing `groupEvery`

Let's define a function to generate a list of groups of `n` sequential elements. For example:

```haskell
groupEvery 3 "longstring"
``` 

would return:

```
["lon", "gst", "rin", "g"]
```

I'm probably missing an already existing one, but the definition is trivial to write so here we go:

```haskell
groupEvery :: Int -> [a] -> [[a]]
groupEvery 0 _ = []
groupEvery e l
    | length l > e  = (take e l) : (groupEvery e (drop e l))
    | otherwise     = [l]
```

####Encoding

Encoding is simple. We just need to:

 - divide the string into groups of 3 letters
 - apply the substitution defined by our map
 - return the string

Let's define a function that takes a well-formed string (with `length == 3` and that exists as a key in the map) and translate it:

```haskell
replaceCodon :: String -> Char
replaceCodon s =
    let (Just x) = Map.lookup s codonTable
    in  x
```

Here we are just getting rid of `Nothing` possibility (that the key is not present in the map) because our preconditions say so. Then encoding is just a matter of wiring up the rest:

```haskell
encode :: String -> String
encode = map replaceCodon . filter ((==3) . length) . groupEvery 3
```
