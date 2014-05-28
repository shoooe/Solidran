###Rabbits and Recurrence Relations

####Understanding the pattern

Taking a blackboard and drawing the example with `n = 5` and `k = 3`, easily leads to the pattern:

```haskell
c = b + k * a
```

assuming `b` to be the previous number and `a` the number before that (a, b, c, ...), in the sequence.
Of course the classical Fibonacci sequence is given by `k = 1`.

####First *iteration*

The first version you can come up with is a basic, more general, version of the fibonacci recursion:

```haskell
func :: Int -> Int -> Int
func n k
    | n < 3     = 1
    | otherwise = (func (n-1) k) + (k * func (n-2) k)
```

In the case where `n < 3` (basically either `1` or `2`, given the preconditions), the function is asking us the first or the second number of the sequence, that we know to be `1` in both cases. 

We can test this by displaying the first 10 digits of the well know fibonacci sequence:

```haskell
fib :: Int -> Int
fib n = func n 1

main = print . map fib $ [1..10]
```

The output is:

```
[1,1,2,3,5,8,13,21,34,55]
```

####The problem

By testing the above solution with the maximum values given (`n = 40` and `k = 5`) it becomes clear the algorithm takes too much time (~3 seconds) to execute. 

The main problem is that we are recursively repeating the calculation of well know solutions. For example given `n = 5` and `k = 2` (consider `F[n]` to be `func n 2`):

```
F[5]
F[4] + k * F[3]
F[3] + k * F[2] + k * (F[2] + k * F[1])
F[2] + k * F[1] + k * F[2] + k * (F[2] + k * F[1])
1 + k + k + k + k * k
1 + 3k + k * k
1 + 6 + 4
11
```

You can see that at some point `F[3]` is calculated 2 times.

####We need to go faster!

I know there's probably a better algorithm, but the one I came up with takes in consideration that we only need two elements of the sequence to calculate the next:

```haskell
nextPair :: Int -> (Int, Int) -> (Int, Int)
nextPair k (a, b) = (b, c)
    where c = b + (k * a)
```

The above function takes a `k` and a pair of ordered numbers in the sequence and returns the next. For example, the following are the pair of numbers starting from `(0, 1)` for `k = 1`:

```
(1, 1)
(1, 2)
(2, 3)
(3, 5)
(5, 8)
```

It's clear that if we need to calculate the third iteration we just need to repeat the call to `nextPair` 3 times. Assuming:

```haskell
fib :: (Int, Int) -> (Int, Int)
fib = nextPair 1
```

then:

```haskell
fib . fib . fib $ (0, 1)
```

will return `(2, 3)`.

Let's define a function that helps us to compose a function n times and apply an initial argument to it:

```haskell
replicateFunc :: (a -> a) -> Int -> a -> a
replicateFunc _  0     start = start
replicateFunc fn count start = replicateFunc fn (count-1) (fn start)
```

The above is some very basic recursion. We pass the function, the number of times we want to repeat it and the first (initial) value.

Now we only need to wire all up into the solution:

```haskell
rabbitsCount :: Int -> Int -> Int
rabbitsCount n k
    | n < 3     = 1
    | otherwise = 
        snd $ replicateFunc fn (n-2) (1, 1)
            where fn = nextPair k
```

We apply our `nextPair` function `n` times with the `k` argument and we just take the second element of the resulting tuple (which is the last one of our calculated sequence).

Let's see how the `n = 5` and `k = 2` example gets calculated this time (given `F` = nextPair 2)

```
snd . F . F . F $ (1, 1)
snd . F . F $ (1, 3)
snd . F $ (3, 5)
snd (5, 11)
11
```

Which is basically the calculation of the sequence from the start (remembering the last 2 elements) until we need to. The most important detail is that we are not calculating anything twice.

####Benchmarking

I've benchmarked both `func` and `rabbitsCount` with Criterion using the following `main`:

```haskell
defaultMain
    [ bench "func 15 3"   $ whnf (func 15) 3
    , bench "func 25 3"   $ whnf (func 25) 3
    , bench "func 35 3"   $ whnf (func 35) 3
    , bench "rabbitsCount 15 3"  $ whnf (rabbitsCount 15) 3
    , bench "rabbitsCount 25 3"  $ whnf (rabbitsCount 25) 3
    , bench "rabbitsCount 35 3"  $ whnf (rabbitsCount 35) 3
    ]
```

I've reduced the samples to `50` per benchmark otherwise the default 100 would have take a lot of time (the thid benchmark, with `func 35 3`, took more than a minute alone).

The results are as follows (I'm only mentioning the mean):

```
func 15 3           -> 91.30113 us
func 25 3           -> 11.39030 ms
func 35 3           -> 1.411508 s

rabbitsCount 15 3   -> 1.114727 us
rabbitsCount 25 3   -> 1.763304 us
rabbitsCount 35 3   -> 2.481532 us
```
