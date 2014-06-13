###[Counting DNA Nucleotides](http://rosalind.info/problems/dna/)

####Counting distinct letters

To count our four letters we are using `countDistinc` contained in `Solidran.List`:

```haskell
nucleoCount :: String -> [Int]
nucleoCount string =
    let map = countDistinct string
    in      [ Map.findWithDefault 0 'A' map
            , Map.findWithDefault 0 'C' map 
            , Map.findWithDefault 0 'G' map 
            , Map.findWithDefault 0 'T' map ]
```

####Showing the results

To show the final list we are going to use the `Output` class, defined in `Solidran.Output`.
