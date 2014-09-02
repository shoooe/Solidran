###[Enumerating Gene Orders](http://rosalind.info/problems/perm/)

####Permutations

Haskell's `Data.List` module contains a very useful function, called `permutations`, that given a list of elements returns a list of list of elements corresponding to the list of all possible permutations with those elements.

As an example, given:

```haskell
permutations :: [a] and we are left with [[a]]
```

we would get results like:

```haskell
permutations "abc"
-- ["abc","bac","cba","bca","cab","acb"]

permutations [1, 2, 3]
-- [[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]
```

These results are exactly what we need.

####The number of permutations

There's one immediate way to calculate the number of permutations for a given list of elements `[1..n]`:

```haskell
length . permutations $ [1..n]
```

or even better:

```haskell
let ps = permutations [1..n]
in  length ps
```

so that we could later reuse the list of permutations.

We are going yo go another way. We are going to apply "combinatorics".

Let's consider a simple example: we have `['a', 'b', 'c']`. 

How many ways can be draw a letter from the original set? 3 times (we could either choose `'a'`, `'b'` or `'c'`).
For each of these times we are left we 2 elements:

1. We choose `'a'` and we are left with `['b', 'c']`
2. We choose `'b'` and we are left with `['a', 'c']`
3. We choose `'c'` and we are left with `['a', 'b']`

At this point we can only choose one of those two elements and we can choose up to 2 different elements:

1. We choose `'a'`, then `'b'` and we are left with `['c']`
2. We choose `'a'`, then `'c'` and we are left with `['b']`
3. We choose `'b'`, then `'a'` and we are left with `['c']`
4. We choose `'b'`, then `'c'` and we are left with `['a']`
5. We choose `'c'`, then `'a'` and we are left with `['b']`
6. We choose `'c'`, then `'b'` and we are left with `['a']`

As you can see the order in which we choose the letters matters, just like in the permutations. That is, `['a', 'b', 'c']` is different from `['b', 'a', 'c']`. So, in the above list, 1 and 3 are different, even if in both we are left with `'c'`.

At that point we are left with only one character and we can only choose that one.

Therefore we have 6 possible permutations: 3 possibilities to pick the first character, then 2 possibilities to pick 2 of the remaining characters and then 1 possibility for the remaining character.

We can apply this reasoning with `n = 4`: we have 4 ways to pick the first character; for each of this way we have 3 ways to pick the second, 2 to pick the third and 1 to pick the last. Therefore we have `4 * 3 * 2 * 1` possible permutations.

For `n = 5` that would be `5 * 4 * 3 * 2 * 1`, or simply `5!` (factorial).
