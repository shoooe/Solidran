hammingDist :: String -> String -> Int
hammingDist a b = foldr fn 0 z
    where 
        fn (a, b) c
            | a == b    = c
            | otherwise = c + 1
        z = zip a b

main :: IO ()
main = do
    c <- getContents
    let [a, b] = lines c
    putStr . show $ hammingDist a b
