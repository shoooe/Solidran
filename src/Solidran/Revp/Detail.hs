module Solidran.Revp.Detail where

import Solidran.Revc.Detail (complementDna)

isReversePalindrome :: String -> Bool
isReversePalindrome s = complementDna s == s

findSequencesOfLengthFrom :: Int -> Int -> [a] -> [(Int, [a])]
findSequencesOfLengthFrom _ _ [] = []
findSequencesOfLengthFrom i n s
    | length s < n  = []
    | otherwise     = (i, current) : recursive
        where 
            current     = take n s
            recursive   = findSequencesOfLengthFrom (i + 1) n (drop 1 s)

findSequencesOfLength :: Int -> [a] -> [(Int, [a])]
findSequencesOfLength = findSequencesOfLengthFrom 1

findReversePalindromes :: [Int] -> String -> [(Int, Int)]
findReversePalindromes ls s =
    let allSequences    = concat . map (\l -> findSequencesOfLength l s) $ ls
        revPSequences   = filter (isReversePalindrome . snd) allSequences
    in map (\(i, ss) -> (i, length ss)) revPSequences
