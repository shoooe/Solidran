###[Counting DNA Nucleotides](http://rosalind.info/problems/dna/)

####The counter

Let's define a data type to old our counts for the letters `A`, `C`, `G` and `T` respectively:

```haskell
data Counter 
    = Counter
        { aCount :: Int
        , cCount :: Int
        , gCount :: Int
        , tCount :: Int }
    deriving (Eq)
```

####Showing results

We are going to need to show this `Counter`, so let's also define a `Show` instance. This is just a matter of taking out 4 numbers and produce a string in the form:

```haskell
"aCount cCount gCount tCount"
```

This can be accomplished using `intercalate`:

```haskell
instance Show Counter where
    show (Counter a c g t) = intercalate " " . map show $ [a, c, g, t]
```

####Counting

The function to count the letters simply increments the corresponding integer of the data object, while reading the string character by character:

```haskell
count :: String -> Counter
count str = foldr incr (Counter 0 0 0 0) str
    where incr 'A' c = c { aCount = aCount c + 1 }
          incr 'C' c = c { cCount = cCount c + 1 }
          incr 'G' c = c { gCount = gCount c + 1 }
          incr 'T' c = c { tCount = tCount c + 1 }
          incr  _  c = c
```

The last pattern match shouldn't be there because the precondition imposes that the string is composed *only* by those 4 characters, but adding isn't difficult.
