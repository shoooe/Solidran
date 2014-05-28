nextPair :: Int -> (Int, Int) -> (Int, Int)
nextPair k (a, b) = (b, c)
    where c = b + (k * a)

replicateFunc :: (a -> a) -> Int -> a -> a
replicateFunc _  0     start = start
replicateFunc fn count start = replicateFunc fn (count-1) (fn start)

rabbitsCount :: Int -> Int -> Int
rabbitsCount n k
    | n < 3     = 1
    | otherwise = 
        snd $ replicateFunc fn (n-2) (1, 1)
            where fn = nextPair k

main = do
    ln <- getLine
    let [n, k] = map read $ words ln
    print $ rabbitsCount n k
