###[Overlap Graphs](http://rosalind.info/problems/grph/)

####A node

Defining a `Node` data type will simplify the way we handle the input, stripping away the part of the DNAstring that we don't care about:

```haskell
data Node
    = Node
        { label     :: String
        , leftEdge  :: String
        , rightEdge :: String }
    deriving (Show, Eq)
```

####Parsing the matrix

The `parseFasta` function takes the integer `k`, referring to `Ok`, and a FASTA `Map String String` (returned by `Fasta.parse`, in `Solidran.Fasta`) and returns a list of `Node`s:

```haskell
parseFasta :: Int -> Map String String -> [Node]
parseFasta i = Map.foldrWithKey parseNode []
    where parseNode k v c = 
              (Node k (take i v) (drop (length v - i) v)) : c
```

The map will contain the label as key and the DNA string as value. We take the first and the last 3 letters of the DNA string and we place is in the left and right edge inside the `Node` object.

####A convenient map

To try and keep the complexity at `O(nlogn)` (as opposed to `O(n^2)`) we place our left edges in a map that will make it `O(logn)` to locate a specific left edge:

```haskell
nodeMap :: [Node] -> Map String [Node]
nodeMap = foldr fn Map.empty
    where fn n m = Map.insertWith (++) (leftEdge n) [n] m
```

####Adjacent nodes

To calculate the list of adjacent nodes we take the map out of the above function, we map over each key-value pair and inside each list of nodes, using our resulting list as a counter of the nested `foldr` calls:

```haskell
nodeAdj :: Map String [Node] -> [(Node, Node)]
nodeAdj m = Map.foldr (\ l c -> foldr fn c l) [] m
    where fn n c = c ++ map (\x -> (n, x)) (fromMaybe [] $ Map.lookup (rightEdge n) m)
```

####Exported function

We are going to export a convenient function `adjacencyList`, which combines everything under the hoods and selectively strips out the node pairs that refer to the same node:

```haskell
adjacencyList :: Int -> String -> [(String, String)]
adjacencyList i = fe . map (\ (n1, n2) -> (label n1, label n2)) . na
    where na = nodeAdj . nodeMap . parseFasta i . Fasta.parse
          fe = filter (\(s1, s2) -> s1 /= s2) 
```

This will provide a `Map`-free interface.
