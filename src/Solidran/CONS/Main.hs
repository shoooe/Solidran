import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intersperse, maximumBy, span)
import Data.Function (on)
import Prelude hiding (showList)

import Shared.List 
    ( rightFoldCol
    , splitBy
    , countElem
    , countFrequency )


type ProfileMat = Map Char [Int]
type Consensus  = [Char]

readInput :: String -> [String]
readInput = filter (/="") . map concat . splitBy ((=='>') . head) . lines

profileMat :: [String] -> ProfileMat
profileMat = rightFoldCol fn iniMap 
    where iniMap = Map.fromList 
              [ ('A', [])
              , ('C', [])
              , ('G', [])
              , ('T', []) ]
          fn col = Map.mapWithKey ((:) . flip countElem col)

mostFrequent :: Eq a => [a] -> [a] -> a
mostFrequent sm = fst . maximumBy (compare `on` snd) . countFrequency sm

consensus :: [String] -> Consensus
consensus = rightFoldCol ((:) . mostFrequent ['A', 'C', 'G', 'T']) ""

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
