###[Rabbits and Recurrence Relations](http://rosalind.info/problems/fib/)

####Understanding the pattern

Taking a blackboard and drawing the example with `n = 5` and `k = 3`, easily leads to the pattern:

```haskell
F[n] = F[n-1] + k * F[n-2]
```

where `F[i]` is the `i`th number of the sequence.

####First *iteration*

The simplest version of the algorithm:

```haskell
func :: Int -> Int -> Int
func n k
    | n < 3     = 1
    | otherwise = (func (n-1) k) + (k * func (n-2) k)
```

is a generalization of the classical fibonacci recursive example.

When `n < 3`, which happens either with `n == 1` or `n == 2` given the preconditions, the result is trivially `1`. In any other case we apply the above formula. 

We can test this by displaying the first 10 digits of the well know fibonacci sequence. Given the Fibonacci sequence:

```haskell
fib :: Int -> Int
fib n = func n 1
```

then:

```haskell
map fib $ [1..10]
```

results in:

```
[1,1,2,3,5,8,13,21,34,55]
```

####The problem

If you test the algorithm with high values (without overflowing) it becomes clear there's something wrong. Using the maximum values from the dataset (`n = 40` and `k = 5`) it takes ~3 seconds to run. 

The main problem is that we are recursively repeating the calculation of well know sequences. For example given `n = 5` and `k = 2` (consider `F[n]` to be `func n 2`):

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

You can see that at some point `F[3]` is calculated 2 times. This is just an example, but it's clear that this waste propagates almost exponentially.

####We need to go faster!

Let's take in consideration that we only need two elements of the sequence to calculate the next. And let's define a function that, given a sequence and `k`, calculates the next sequence:

```haskell
nextPair :: Int -> (Int, Int) -> (Int, Int)
nextPair k (a, b) = (b, c)
    where c = b + (k * a)
```

As an example, for `k = 1` and starting from `(0, 1)` the sequence of pairs would be:

```
(1, 1)
(1, 2)
(2, 3)
(3, 5)
(5, 8)
```

If we need to calculate the nth iteration, we just need to repeat the call to `nextPair` n times. 

Assuming the Fibonacci sequence:

```haskell
fib :: (Int, Int) -> (Int, Int)
fib = nextPair 1
```

then:

```haskell
fib . fib . fib $ (0, 1)
```

will return `(2, 3)`, which is correctly the 3rd and 4th elements of the sequence.

Given our `composeN` function (located in `Solidran.Function`), that composes a function `n` times, we just need to wire it all up into the solution:

```haskell
rabbitsCount :: Int -> Int -> Int
rabbitsCount n k
    | n < 3     = 1
    | otherwise = snd $ composeN (nextPair k) (n-2) (1, 1)
```

We apply our `nextPair` function `n` times, with the `k` argument, and take the second element of the resulting tuple (which is the last one of our calculated sequence).

Let's see how the `n = 5` and `k = 2` example gets calculated this time (given `F = nextPair 2`)

```
snd . F . F . F $ (1, 1)
snd . F . F $ (1, 3)
snd . F $ (3, 5)
snd (5, 11)
11
```

The most important detail is that we are not calculating anything twice.

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

I've reduced the samples to `50` per benchmark, otherwise the default 100 would have taken too much time (`func 35 3` took more than a minute alone).

The results are as follows (the value displayed is the mean):

```
func 15 3           -> 91.30113 us
func 25 3           -> 11.39030 ms
func 35 3           -> 1.411508 s

rabbitsCount 15 3   -> 1.114727 us
rabbitsCount 25 3   -> 1.763304 us
rabbitsCount 35 3   -> 2.481532 us
```
