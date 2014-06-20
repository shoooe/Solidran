#Solidran

This folder contains shared functions and utilities.

##List

####The `splitBy` function

Takes a predicate and splits the given list, considering the elements that satisfy the given predicate as pivots:

```haskell
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy c ls = 
    let (l, r) = span (not . c) ls
    in l : (splitBy c . drop 1 $ r)
```

####The `countDistinct` function

This function takes a list of elements and returns the `Map a Int` that *maps* each element to the number of times it is repeated inside the list:

```haskell
countDistinct :: Ord a => [a] -> Map a Int
countDistinct ls = foldr fn Map.empty ls
    where fn e map = Map.insertWith (+) e 1 map
```

The complexity is `O(n * log(k))` with `n` begin the size of the list and `k` being the number of different elements in the list. For most of our applications (in which `k == 4`) the actual complexity is `O(n)`.

####The `countIf` function

Traverses the list and keeps count of how many elements satisfy the given predicate:


```haskell
countIf :: (a -> Bool) -> [a] -> Int
countIf _ [] = 0
countIf p (x:xs) = c + countIf p xs
    where c = if p x then 1 else 0
```

####The `groupEvery` function

It's an utility function that takes a integer `n` and splits the given list into chunks of `n` elements. The only exception is for the last group of the list which can potentially be less than `n` elements long, if the initial number of elements is not divisible by `n`.

```haskell
groupEvery :: Int -> [a] -> [[a]]
groupEvery 0 _ = []
groupEvery e l
    | length l > e  = (take e l) : (groupEvery e (drop e l))
    | otherwise     = [l]
```

##Output

####The `Output` class

This class defines all the elements that can be rendered (in the way Rosalind expects) as output. The only function defined in the class is `output` which, similarly for `show`, takes an object and returns its `String` representation.

##Function

####The `composeN` function

This function takes a number `n` and a function from any type to the same type (due to function composition rules) and composes the function `n` times with itself.

##Fasta

#####The `parse` function

The input is given us in the form:

```
>1
a
b
…
>2
c
d
…
…
```

which can be seen as:

```
>1\na\nb\n…
>2\nc\nd\n…
…
```

First of all we can separate each record by splitting the string over `>`. That will lead our input to be:

```haskell
[ ""
, "1\na\nb\n…"
, "2\nc\nd\n…"
, …
]
```

Since we don't consider empty strings to be important, we can just discard them. 

Then we need to parse each line of the above list by taking the guaranteed string (the label) before the first `\n` and the rest. We basically need a function that takes a string in the format:

```haskell
"1\na\nb\n…"
```

and translates it into:

```haskell
("1" "ab…")
```
and here it is:

```haskell
parseSingle :: String -> (String, String)
parseSingle inp =
    let (l:r) = lines inp
    in  (l, concat r)
```

The above function splits the string over `\n`, takes the first element (the label) and concatenates the rest.

Mapping the above function ober the splitted (on `>`) list gives us the resulting `Map`:

```haskell
parse :: String -> Map String String
parse inp = 
    let bks = filter (/="") . splitBy (=='>') $ inp
    in  Map.fromList (map parseSingle bks)
```
