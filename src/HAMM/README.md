###[Counting Point Mutations](http://rosalind.info/problems/hamm/)

####Zip

We want to compare every characters of both of the strings in pair. Haskell provides the `zip` function to group elements of two lists together into pairs. So for example:

```haskell
zip [1, 2, 3] [4, 5, 6]
```

will result in:

```haskell
[(1, 4), (2, 5), (3, 6)]
```

String of course are lists. Given the two strings in the example:

```haskell
zip
    ['G', 'A', 'G', 'C', 'C', 'T', 'A', 'C', 'T', 'A', 'A', 'C', 'G', 'G', 'G', 'A', 'T']
    ['C', 'A', 'T', 'C', 'G', 'T', 'A', 'A', 'T', 'G', 'A', 'C', 'G', 'G', 'C', 'C', 'T']
```

the zip will be:

```haskell
[ ('G','C')     -- 1
, ('A','A')
, ('G','T')     -- 2
, ('C','C')
, ('C','G')     -- 3
, ('T','T')
, ('A','A')
, ('C','A')     -- 4
, ('T','T')
, ('A','G')     -- 5
, ('A','A')
, ('C','C')
, ('G','G')
, ('G','G')
, ('G','C')     -- 6
, ('A','C')     -- 7
, ('T','T') ]
```

Now it's just a matter of applying our beloved `foldr` over that list with a function that increments the counter only if the two characters of the pair are different:

```haskell
hammingDist :: String -> String -> Int
hammingDist a b = foldr fn 0 z
    where 
        fn (a, b) c
            | a == b    = c
            | otherwise = c + 1
        z = zip a b
```
