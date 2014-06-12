import Data.List (isPrefixOf, intercalate)

findOccs :: Eq a => [a] -> [a] -> [Int]
findOccs n h = filter fn ixs
    where fn i  = isPrefixOf n . drop (i-1) $ h
          ixs   = [1..((length h - length n) + 1)]

showOccs :: [Int] -> String
showOccs is = intercalate " " . map show $ is

main :: IO ()
main = do
    cont <- getContents
    let [haystack, needle] = lines cont
    putStrLn . showOccs $ findOccs needle haystack 
