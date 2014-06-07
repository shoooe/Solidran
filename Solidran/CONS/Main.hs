import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intersperse, maximumBy, span)
import Data.Function (on)
import Prelude hiding (showList)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy c ls = 
    let (l, r) = span (not . c) ls
    in l : (splitBy c . drop 1 $ r)

readInput :: String -> [String]
readInput = filter (/="") . map concat . splitBy ((=='>') . head) . lines

foldRCol :: ([a] -> b -> b) -> b -> [[a]] -> b
foldRCol fn c l =
    if null heads 
        then c
        else heads `fn` foldRCol fn c tails
    where heads = concat . map (take 1) $ l
          tails = map (drop 1) l

countElem :: Eq a => [a] -> a -> Int
countElem ls e = length $ filter (== e) ls

mostFrequent :: Eq a => [a] -> [a] -> a
mostFrequent sm ls = fst $ maximumBy (compare `on` snd) $ map fn sm
    where fn e = (e, length $ filter (==e) ls)

type ProfileMat = Map Char [Int]
type Consensus  = [Char]

profileMat :: [String] -> ProfileMat
profileMat = foldRCol fn iniMap 
    where iniMap = Map.fromList 
              [ ('A', [])
              , ('C', [])
              , ('G', [])
              , ('T', []) ]
          fn col = Map.mapWithKey ((:) . countElem col)

consensus :: [String] -> Consensus
consensus = foldRCol ((:) . mostFrequent ['A', 'C', 'G', 'T']) ""

showList :: [Int] -> String
showList l = concat . intersperse " " . map show $ l

showMap :: Map Char [Int] -> String
showMap = Map.foldrWithKey fn ""
    where fn k v c = concat [[k], ": ", showList v, "\n", c]

main :: IO ()
main = do
    c <- getContents
    let ls = readInput c
    putStrLn . consensus $ ls
    putStr . showMap . profileMat $ ls
