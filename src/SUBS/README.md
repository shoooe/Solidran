###[Finding a Motif in DNA](http://rosalind.info/problems/subs/)

####Finding indexes

We will use the following idea:
 
 - take all possible indexes
 - filter the list by those that are index of a substring

To accomplish this we can use `filter` with our function that checks if a valid substring starts at that index. The range of possible indexes clearly goes from `1` to `length h - length n + 1`; that is: the "haystack" string cannot be shorter than the "needle" string:

```haskell
findOccs :: Eq a => [a] -> [a] -> [Int]
findOccs n h = filter fn ixs
    where fn i  = isPrefixOf n . drop (i-1) $ h
          ixs   = [1..((length h - length n) + 1)]
```

####Showing indexes

To show our indexes we just need to intercalate a space between them:

```haskell
showOccs :: [Int] -> String
showOccs is = intercalate " " . map show $ is
```
