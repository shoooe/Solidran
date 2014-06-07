###[Mendel's First Law](http://rosalind.info/problems/iprb/)

####Probability tree

Given:

```
HD = homozygous dominant
HR = homozygous recessive 
HE = heterozygous

k  = individuals HD
n  = individuals HR
m  = individuals HE
j  = k + n + m
```

the tree diagram for this problem is not particularly hard to draw:

<p align="center">
	<img src="probability_tree.png" alt="The probability tree diagram"/>
</p>

The overall idea of our program will be: given `k`, `m` and `n`, calculate the probability of each branch (from root to leaves) and then sum them all together.

####Organisms and mating

Let's begin by defining the organism types:

```haskell
data OrgType 
    = HomoDom
    | Hetero
    | HomoRec
    deriving (Eq, Ord)
```

and the type for representing the probability:

```haskell
type Prob = Double
```

I propose this function for the probability that, given two organism types, the resulting organism has at least 1 dominant allele:

```haskell
probDom :: OrgType -> OrgType -> Prob
probDom Hetero Hetero   = 3 / 4
probDom Hetero HomoRec  = 2 / 4
probDom HomoRec Hetero  = 2 / 4
probDom HomoRec HomoRec = 0.0
probDom _       _       = 1.0
```

This version is not good, because: 1) does too many things 2) unnecessary magic numbers 3) everything else. 

We can improve this by dividing the "idea" of mating into a specific function in the form:

```haskell
mate :: OrgType -> OrgType -> [OrgType]
```

The function would take two organism types and return 4 corresponding to the possible 4 outcomes of mating those two arguments.

In the writing of this you'll realize you'll have to refactor your `OrgType` by defining a single `OrgType` data constructor and let it accept two `Allele`:

```haskell
data Allele
    = Dominant
    | Recessive
    deriving (Eq, Ord)

data OrgType
    = OrgType Allele Allele
    deriving (Eq, Ord)
```

Then you could define a simple function to detect if in an `OrgType` there's at least 1 dominant allele:

```haskell
hasDominant :: OrgType -> Bool
hasDominant (OrgType a b)
    | a == Dominant || b == Dominant = True
    | otherwise                      = False
```

and finally define our `probDom` function as:

```haskell
count :: a -> [a] -> Int
count x = length . filter (==x)

probDom :: OrgType -> OrgType -> Prob
probDom a b = 
    let l = map hasDominant $ mate a b
    in  fromIntegral (count True l) / fromIntegral (length l)
```

This is not necessary for this particular program, but it's good to notice.

####Stateful computation

To calculate the probability that each branch (given two `OrgType`s, `x` and `y` respectively) leads to an organism with at least a dominant allele, we can imagine the following steps:

 1. Calculate the probability of drawing an `x` and save it to `px`, then draw `x`
 2. Calculate the probability of that, after drawing `x`, `y` is drawn and save it to `py`
 3. Apply our `probDom` on `x` and `y` to get `pd`
 4. Multiply them all together

Give our "environment" to be the mapping of an organism type and it's count:

```haskell
type Env = Map OrgType Int
```

The prototype for this function is:

```haskell
branch :: OrgType -> OrgType -> State Env Prob
branch x y = do
    px <- prob x
    draw x
    py <- prob y
    draw y -- not needed
    let pd = probDom x y
    return $ px * py * pd
```

The `State` monad is choosen mainly because we need to express a stateful environment that changed when we draw an `OrgType` from the environment.

####Probability and drawing

Now we just need to implement `prob` and `draw`.

The function `prob` calculates the probability that a given `OrgType` is drawn from the environment. This function does not require a state per se, but fits well in the `branch` stateful context:

```haskell
prob :: OrgType -> State Env Prob
prob o = do
    m <- State.get
    let (Just no) = Map.lookup o m
    let tot = Map.foldr (+) 0 m
    if tot /= 0
        then return $ (fromIntegral no) / (fromIntegral tot)
        else return 0
```

There are few things to notice from this function:

 - We don't accept an `OrgType` that is not definined in the environment.
 - We accept an environement with no organisms.

Both preconditions are met in our specific domain, so let's move on.

The `draw` function just updated the environement the way we expect it to:

```haskell
draw :: OrgType -> State Env ()
draw o = do
    State.modify $ Map.adjust (subtract 1) o
```

####The sum

Given:

```haskell
orgList :: [OrgType]
orgList = [HomoDom, Hetero, HomoRec]
```

we can use a list comprehension to draw from `orgList` two times and generate the product of:

```
orgList X orgList
```

that is all the mating possibilities in our environment (every possible branch of our original tree).

The total probability is then trivially given by the sum of all of them:

```haskell
totalProb :: Env -> Prob
totalProb e = sum [State.evalState (branch x y) e | x <- orgList, y <- orgList]
```
