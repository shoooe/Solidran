###[Inferring mRNA from Protein](http://rosalind.info/problems/mrna/)

####Codon table

First of all we need to count how many way to express a specific amino acids (character) can be codificated. We are going to group the letters by the number of possible codons that codify them:

```haskell
combsFor :: Char -> Int
combsFor c
    | c `elem` "WM"         = 1
    | c `elem` "FYCHQNKDE"  = 2
    | c `elem` "I\0"        = 3
    | c `elem` "PTVAG"      = 4
    | c `elem` "LSR"        = 6
    | otherwise             = undefined
```

####Possible combinations

To calculate the total number of possible combinations of codons for a given string we can just traverse the string and multiply our current counter (starting from `1`) for the number of combinations of each letter (`combsFor`).

Notice that we start from `3` because there's an implicit `\0` at the end of each string, and that character ("Stop") is codificated in three possible ways.

It would have been the same if we had just appended `\0` at the end of the string and started from `1`. We know, though, that appending a character at the end of a `String` takes O(n). Starting from `3` costs us nothing.

Another solution would be to work with `ByteString`s, in which appending is an O(1) operation.

```haskell
rnaCombsMod :: Int -> String -> Int
rnaCombsMod m str =
    foldr (\c i ->
        let ni = i * combsFor c
        in  ni `mod` m) 3 str
```
