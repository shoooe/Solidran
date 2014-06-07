##Shared libraries

###`Shared.List`

This module contains all the functions that operate on lists. Most of them can be extended with a more generic `Foldable` or `Traversable`. 
The current version is a good compromise between simplicity and functionality.

####`splitBy`

This function takes a list of elements and splits them by a predicate. The behavior mimics: 

```haskell
splitBy ',' "1,2,3,4"
-- ['1', '2', '3', '4']

splitBy ',' ""
-- []
```

and here's the implementation:

```haskell
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy c ls = 
    let (l, r) = span (not . c) ls
    in l : (splitBy c . drop 1 $ r)
```

We are using `span` to divide the list.
The remaining list (the second element of the tuple) contains the element that satisfied the predicate in first position, therefore we need to call `drop 1` on it before the recursive call.

####`rightFoldCol`

This function behaves like `foldr` except that it traverses a list of lists by their columns:

```haskell
rightFoldCol :: ([a] -> b -> b) -> b -> [[a]] -> b
rightFoldCol fn c l =
    if null heads 
        then c
        else heads `fn` rightFoldCol fn c tails
    where heads = concat . map (take 1) $ l
          tails = map (drop 1) l
```

####`countElem`

The following function is useful to count the occurrences of an element in a list:

```haskell
countElem :: Eq a => [a] -> a -> Int
countElem ls e = length $ filter (== e) ls
```

####`countFrequency`

Used to count the frequencies of certain elements over a list. The function requires a list of element to reduce the complexity of the algorithm.

```haskell
countFrequency :: Eq a => [a] -> [a] -> a
countFrequency sm ls = map fn sm
    where fn e = (e, countElem e ls)
```
