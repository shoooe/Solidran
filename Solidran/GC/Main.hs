{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad (join)

data Record = Record Text Text
    deriving (Eq, Show)

data Result = Result Text Double
    deriving (Eq, Show)

instance Ord Result where
    compare (Result _ a) (Result _ b) = compare a b

showResult :: Result -> String
showResult (Result str per) = concat [Text.unpack str, "\n", show per]

countLetters :: [Char] -> Text -> (Int, Int)
countLetters lets str = Text.foldr fn (0, 0) str
    where 
        fn chr (c,o) = 
            if (==chr) `any` lets
                then (c+1, o+1)
                else (c  , o+1)

calcPercent :: (Int, Int) -> Double
calcPercent (a, b) = (fromIntegral a * 100) / fromIntegral b

calcResult :: Record -> Result
calcResult (Record nm str) =
    Result nm (calcPercent . countLetters ['C', 'G'] $ str)

parseRecord :: Text -> Record
parseRecord str = 
    let (nm:rest) = Text.lines str
    in Record nm (Text.concat rest)

parseInput :: Text -> [Record]
parseInput str = map parseRecord . filter (/="") $ Text.split (=='>') str

showOutput :: String -> String
showOutput str =
    let res = map calcResult . parseInput . Text.pack $ str
    in showResult . maximum $ res

main = getContents >>= (putStr . showOutput)
