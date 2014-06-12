import Data.Map (Map)
import qualified Data.Map as Map

codonTable :: Map String Char
codonTable =
    Map.fromList
        [ ("UUU", 'F')
        , ("CUU", 'L')
        , ("AUU", 'I')
        , ("GUU", 'V')
        , ("UUC", 'F')
        , ("CUC", 'L')
        , ("AUC", 'I')
        , ("GUC", 'V')
        , ("UUA", 'L')
        , ("CUA", 'L')
        , ("AUA", 'I')
        , ("GUA", 'V')
        , ("UUG", 'L')
        , ("CUG", 'L')
        , ("AUG", 'M')
        , ("GUG", 'V')
        , ("UCU", 'S')
        , ("CCU", 'P')
        , ("ACU", 'T')
        , ("GCU", 'A')
        , ("UCC", 'S')
        , ("CCC", 'P')
        , ("ACC", 'T')
        , ("GCC", 'A')
        , ("UCA", 'S')
        , ("CCA", 'P')
        , ("ACA", 'T')
        , ("GCA", 'A')
        , ("UCG", 'S')
        , ("CCG", 'P')
        , ("ACG", 'T')
        , ("GCG", 'A')
        , ("UAU", 'Y')
        , ("CAU", 'H')
        , ("AAU", 'N')
        , ("GAU", 'D')
        , ("UAC", 'Y')
        , ("CAC", 'H')
        , ("AAC", 'N')
        , ("GAC", 'D')
        , ("UAA", '\0')
        , ("CAA", 'Q')
        , ("AAA", 'K')
        , ("GAA", 'E')
        , ("UAG", '\0')
        , ("CAG", 'Q')
        , ("AAG", 'K')
        , ("GAG", 'E')
        , ("UGU", 'C')
        , ("CGU", 'R')
        , ("AGU", 'S')
        , ("GGU", 'G')
        , ("UGC", 'C')
        , ("CGC", 'R')
        , ("AGC", 'S')
        , ("GGC", 'G')
        , ("UGA", '\0')
        , ("CGA", 'R')
        , ("AGA", 'R')
        , ("GGA", 'G')
        , ("UGG", 'W')
        , ("CGG", 'R')
        , ("AGG", 'R')
        , ("GGG", 'G') ]

groupEvery :: Int -> [a] -> [[a]]
groupEvery 0 _ = []
groupEvery e l
    | length l > e  = (take e l) : (groupEvery e (drop e l))
    | otherwise     = [l]

replaceCodon :: String -> Char
replaceCodon s =
    let (Just x) = Map.lookup s codonTable
    in  x

encode :: String -> String
encode = map replaceCodon . filter ((==3) . length) . groupEvery 3

main :: IO ()
main = getLine >>= (putStrLn . encode)
