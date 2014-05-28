dnaToRna :: String -> String
dnaToRna = map toU
    where
        toU 'T' = 'U'
        toU  c  =  c

main :: IO ()
main = getLine >>= putStr . dnaToRna
