###[Locating Restriction Sites](http://rosalind.info/problems/revp/)

####Splitting the problem

To split the problem I've decided to go with a slighly less performant solution, but more reusable and clean interface. The result was delivered in a couple of seconds on a fairly low hardware (2,4 GHz Intel Core 2 Duo + 4GB of RAM + "3MB on-chip shared L2 cache").

The most performant solution would clearly be to traverse the string, each time leaving 1 character behind (`drop`), extract the strings of length from 4 to 12, and remember the current index and the length of those that satisfy the predicate.

####The reusable components

To define the predicate we can reuse a function from the REVP problem:

```haskell
import Solidran.Revc.Detail (complementDna)

isReversePalindrome :: String -> Bool
isReversePalindrome s = complementDna s == s
```

Then we just need to define a function extracts all the substrings of a given length, returning the string itself (that will be passed to the filter) and the index (context) in which it was found:

```haskell
findSequencesOfLengthFrom :: Int -> Int -> [a] -> [(Int, [a])]
findSequencesOfLengthFrom _ _ [] = []
findSequencesOfLengthFrom i n s
    | length s < n  = []
    | otherwise     = (i, current) : recursive
        where 
            current     = take n s
            recursive   = findSequencesOfLengthFrom (i + 1) n (drop 1 s)

findSequencesOfLength :: Int -> [a] -> [(Int, [a])]
findSequencesOfLength = findSequencesOfLengthFrom 1
```

At this point we can glue all together by finding all substrings, keeping on those that satisfy our predicate and then transforming the string we have (that we don't need anymore) in its length. I know we are recalculating the length again for apparently no reason, but also returning the length from the `findSequencesOfLength` would be:

 1. redundant
 2. counter intuitive (what would be the order of the index, the string and the length in the tuple?)

the end solution is just 3 lines of code:

```
findReversePalindromes :: [Int] -> String -> [(Int, Int)]
findReversePalindromes ls s =
    let allSequences    = concat . map (\l -> findSequencesOfLength l s) $ ls
        revPSequences   = filter (isReversePalindrome . snd) allSequences
    in map (\(i, ss) -> (i, length ss)) revPSequences
```
