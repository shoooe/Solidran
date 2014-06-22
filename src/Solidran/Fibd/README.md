###[Mortal Fibonacci Rabbits](http://rosalind.info/problems/fibd/)

####The growth

The grow at each step is clearly determined by considering that:

 - the number of newborns of the precededing step is preserved (the newborns just mature and don't create new pairs)
 - those that die are those pairs that were newborn `m` months ago; also they mate once before dying
 - those that were old in the previous step and do not die will produce double their number of pairs (that is, they'll live and mate to generate another pair)

With this considerations we come up with the following sequence (considering `T` = total, `N` = newborns, `O` = non-newborns:

```
T[n-1] = N[n-1] + (O[n-1] - N[n-m]) * 2 + N[n-m]
```

Specifically:

- `N[n-1]` = newborns of the previous step (which just mature)
- `(O[n-1] - N[n-m]) * 2` = non-newborn rabbits of the previous step that do not die (which produce another pair)
- `N[n-m]` = non-newborn rabbits of the previous step that do die (which just produce another pair that replace themselves)

The above sequence can be simplified:

```
T[n-1] = N[n-1] + (O[n-1] - N[n-m]) * 2 + N[n-m]
T[n-1] = N[n-1] + O[n-1] * 2 - N[n-m] * 2 + N[n-m]
T[n-1] = N[n-1] + O[n-1] * 2 - N[n-m]
```
####Memoization

Considering that we have to be able to go back `m` times in the sequence to retrieve those that are supposed to die at each step, we shall consider *memoization* (the technique of saving subproblems in order to calculate superproblems).

List is not a great choice, as going back `m` will cost us `O(m)` time. On the other hand, `Vector` of the `vector` standard package is an excellent choice (allows `O(1)` indexing).

We are going to save the total number of pairs of rabbits **and** the number of new born of each step, starting with `1` newborn.

For this we are going to define a simple `Step` data structure:

```haskell
data Step
    = Step
        { total     :: Integer
        , newborn   :: Integer }
```

With the above informations we are able to calculate the old pairs as well.

####Next step

To calculate the next step of the sequence we consider two cases:

 - we don't have started yet, in which case we are at the beginning of the sequence and we will return `Step 1 1`
 - otherwise we need to apply the above formula

And here's our `calcNext` function that takes `m` and the already calculated vector and just builds a new one with the next step on the front:

```haskell
calcNext :: Int -> Vector Step -> Step
calcNext m vec =
    let n = Vector.length vec
        new = newborn (vec ! (n - 1))
        old = total (vec ! (n - 1)) - newborn (vec ! (n - 1))
        die = newborn $ fromMaybe (Step 0 0) (vec !? (n - m))
    in case n of
        0           -> Step 1 1
        otherwise   -> Step (new + old * 2 - die) old
```

####Building the whole vector

`Vector` gives us just the right function to apply `calcNext m` to: `constructN`. This functions builds the vector progressively applying a function that adds a new element at every step. The complexity is guaranteed to be `O(n)`. Perfect:

```haskell
sequenceTable :: Int -> Int -> Vector Step
sequenceTable n m =
    Vector.constructN n (calcNext m)
```

Now it's just a matter of retrieving the right information from the last element of the vector:

```haskell
countRabbits :: Int -> Int -> Integer
countRabbits n m = total . Vector.last $ sequenceTable n m
```
