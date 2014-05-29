###[Transcribing DNA into RNA](http://rosalind.info/problems/rna/)

####Replacing T and U

The proposed function is probably not the most concise, but it's easy to read:

```haskell
dnaToRna :: String -> String
dnaToRna = map toU
    where
        toU 'T' = 'U'
        toU  c  =  c
```
