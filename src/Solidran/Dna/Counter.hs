module Solidran.Dna.Counter 
    ( Counter(..)
    , count
    ) where

import Data.List (intercalate) 

data Counter 
    = Counter
        { aCount :: Int
        , cCount :: Int
        , gCount :: Int
        , tCount :: Int }
    deriving (Eq)

instance Show Counter where
    show (Counter a c g t) = intercalate " " . map show $ [a, c, g, t]

count :: String -> Counter
count str = foldr incr (Counter 0 0 0 0) str
    where incr 'A' c = c { aCount = aCount c + 1 }
          incr 'C' c = c { cCount = cCount c + 1 }
          incr 'G' c = c { gCount = gCount c + 1 }
          incr 'T' c = c { tCount = tCount c + 1 }
          incr  _  c = c
