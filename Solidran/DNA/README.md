###[Counting DNA Nucleotides](http://rosalind.info/problems/dna/)

####Counting the letters

We can use a 4-tuple to represent the counts of `A`, `C`, `G` and `T` respectively:

```haskell
type Count  = (Int, Int, Int, Int)
```

The function to count the letters simply (and verbosely) increments the corresponding integer of the tuple while reading the string character by character:

```haskell
countLetters :: String -> Count
countLetters str = foldr incr (0, 0, 0, 0) str
    where 
        incr 'A' (a, c, g, t) = (a + 1, c, g, t)
        incr 'C' (a, c, g, t) = (a, c + 1, g, t)
        incr 'G' (a, c, g, t) = (a, c, g + 1, t)
        incr 'T' (a, c, g, t) = (a, c, g, t + 1)
        incr  _   c           = c
```

The last pattern match shouldn't be there because the precondition imposes that the string is composed *only* by those 4 characters, but adding isn't difficult.

####Showing the results

Showing the result is just a matter of taking the 4-tuple:

```haskell
(a, b, c, d)
```
and produce a string:

```haskell
"a b c d"
```

This can be accomplished using `show` and `intercalate`:

```haskell
showResult :: Count -> IO ()
showResult (a, c, g, t) = putStr . intercalate " " . map show $ [a, c, g, t]
```
