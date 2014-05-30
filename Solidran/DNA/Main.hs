import Data.List (intercalate) 

type Count  = (Int, Int, Int, Int)

countLetters :: String -> Count
countLetters str = foldr incr (0, 0, 0, 0) str
    where 
        incr 'A' (a, c, g, t) = (a + 1, c, g, t)
        incr 'C' (a, c, g, t) = (a, c + 1, g, t)
        incr 'G' (a, c, g, t) = (a, c, g + 1, t)
        incr 'T' (a, c, g, t) = (a, c, g, t + 1)
        incr  _   c           = c

showResult :: Count -> IO ()
showResult (a, c, g, t) = putStr . intercalate " " . map show $ [a, c, g, t]

main :: IO ()
main = getLine >>= showResult . countLetters
