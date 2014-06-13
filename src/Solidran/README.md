#Solidran

This folder contains shared functions and utilities.

##List

####The `countDistinct` function

This function takes a list of elements and returns the `Map a Int` that *maps* each element to the number of times it is repeated inside the list:

```haskell
countDistinct :: Ord a => [a] -> Map a Int
countDistinct ls = foldr fn Map.empty ls
    where fn e map = Map.insertWith (+) e 1 map
```

The complexity is `O(n * log(k))` with `n` begin the size of the list and `k` being the number of different elements in the list. For most of our applications (in which `k == 4`) the actual complexity is `O(n)`.

##Output

####The `Output` class

This class defines all the elements that can be rendered (in the way Rosalind expects) as output. The only function defined in the class is `output` which, similarly for `show`, takes an object and returns its `String` representation.
