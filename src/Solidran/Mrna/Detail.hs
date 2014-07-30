module Solidran.Mrna.Detail (rnaCombsMod) where

combsFor :: Char -> Int
combsFor c
    | c `elem` "WM"         = 1
    | c `elem` "FYCHQNKDE"  = 2
    | c `elem` "I\0"        = 3
    | c `elem` "PTVAG"      = 4
    | c `elem` "LSR"        = 6
    | otherwise             = undefined

rnaCombsMod :: Int -> String -> Int
rnaCombsMod m str =
    foldr (\c i ->
        let ni = i * combsFor c
        in  ni `mod` m) 3 str
