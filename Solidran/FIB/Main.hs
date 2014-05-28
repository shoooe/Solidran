-- Given a function, a number n and an initial
-- value, it combines the function n times and
-- calls it with the initial value.
--
replicateFunc :: (a -> a) -> Int -> a -> a
replicateFunc _  0     start = start
replicateFunc fn count start = replicateFunc fn (count-1) (fn start)

-- Given the first two numbers of the sequence, n and k,
-- it returns the next pair of the sequence.
--
-- As an example, these are the first pairs of the
-- fibonacci sequence:
--      (1, 1)
--      (1, 2)
--      (2, 3)
--      (3, 5)
--      (5, 8)
--
nextPair :: Int -> (Int, Int) -> (Int, Int)
nextPair k (a, b) = (b, c)
    where c = b + (k * a)

rabbitsCount :: Int -> Int -> Int
rabbitsCount n k
    | n < 2     = 1
    | otherwise = 
        snd $ replicateFunc fn (n-2) (1, 1)
            where fn        = nextPair k

main = do
    ln <- getLine
    let [n, k] = map read $ words ln
    print $ rabbitsCount n k
