complementDna :: String -> String
complementDna = reverse . map comp
    where
        comp 'A' = 'T'
        comp 'T' = 'A'
        comp 'C' = 'G'
        comp 'G' = 'C'

main :: IO ()
main = getLine >>= putStr . complementDna
