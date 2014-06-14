module Solidran.Output where

import Data.List (intersperse)

class Show a => Output a where
    output :: a -> String
    output = show

instance Output Int
instance Output Integer
instance Output Double
instance Output Float

instance Output Char where
    output c = [c]

instance Output a => Output [a] where
    output = concat . intersperse " " . map output
