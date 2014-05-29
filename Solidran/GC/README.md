###[Computing GC Content](http://rosalind.info/problems/gc/)

####Divide and conquer

The problem is divided in two separated parts: parsing the input, calculating the result. Both of them are equally interesting and "difficult".

####Our data

We can define two type of entities in the domain of the problem:

 - records (what is given us)
 - results (what we want to show)

Of the records we care about the label and the string:

```haskell
data Record = Record Text Text
    deriving (Eq, Show)
```

Of the results all we care about is the label and the percentage of `C` and `G` in the string:

```haskell
data Result = Result Text Double
    deriving (Eq, Show)
```

You can also define a cleaner record syntax:

```haskell
data Record = 
    Record 
        { label  :: Text 
        , string :: Text }
    deriving (Eq, Show)

data Result = 
    Result
        { name       :: Text 
        , percentage :: Double }
    deriving (Eq, Show)
```

but, at the end of the day, we are not going to use `label`, `string`, `name` and `percentage`.

We know we will order `Result`s, so let's define an instance of `Ord` for them:

```haskell
instance Ord Result where
    compare (Result _ a) (Result _ b) = compare a b
```

####Showing and parsing results

We have two choices here:

 - define an instance of `Show`
 - define our own function

Both gets the job done, but when it comes to `read` and `show` I'd like to think of them as linked by the following relation:

```haskell
(read . show $ x) == x
```

Our parsing function will not read the same kind of format that we are going to show. Therefore:

```haskell
showResult :: Result -> String
showResult (Result str per) = concat [Text.unpack str, "\n", show per]
```

####Calculating the result

We first need to count how many occurences of `C` and `G` there are in our string. Then we have to find the length of the string. Let's do both in a single passage:

```haskell
countLetters :: [Char] -> Text -> (Int, Int)
countLetters lets str = Text.foldr fn (0, 0) str
    where 
        fn chr (c,o) = 
            if (==chr) `any` lets
                then (c+1, o+1)
                else (c  , o+1)
```

Now that we have the number of occurences and the total lenght of the string, finding the percentage is trivial:

```haskell
calcPercent :: (Int, Int) -> Double
calcPercent (a, b) = (fromIntegral a * 100) / fromIntegral b
```

Let's wire it all up in a function that will take a `Record` from the parsed input and calculate a `Result` to be shown:

```haskell
calcResult :: Record -> Result
calcResult (Record nm str) =
    Result nm (calcPercent . countLetters ['C', 'G'] $ str)
```

####Parsing the input

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

First of all we can separate each `Result` by splitting the string over `>`. That will lead out input to be:

```haskell
[ ""
, "1\na\nb\n…"
, "2\nc\nd\n…"
, …
]
```

Since we don't consider empty strings to be important, we can just discard them. And here's the function:

```haskell
parseInput :: Text -> [Record]
parseInput str = map parseRecord . filter (/="") $ Text.split (=='>') str
```

Now we need a function `parseRecord` that takes a string in the format:

```haskell
"1\na\nb\n…"
```

and translates it into:

```haskell
Record "1" "ab…"
```

We can just split by `\n`, take the first result of the resulting string (the label) and join the rest together (effectively eliminating the occurences of `\n`):

```haskell
parseRecord :: Text -> Record
parseRecord str = 
    let (nm:rest) = Text.lines str
    in Record nm (Text.concat rest)
```

####Wiring up

The final function looks like this:

```haskell
showOutput :: String -> String
showOutput str =
    let res = map calcResult . parseInput . Text.pack $ str
    in showResult . maximum $ res
```
