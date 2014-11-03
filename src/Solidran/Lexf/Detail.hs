module Solidran.Lexf.Detail (getAlphabetStrings) where

import Control.Monad (liftM2)

getAlphabetStrings :: Int -> [Char] -> [String]
getAlphabetStrings n a = foldr (liftM2 (:)) [""] (replicate n (a :: [Char]))
