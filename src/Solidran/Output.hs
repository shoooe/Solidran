{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Solidran.Output where

import Data.List (intersperse)

class Output a where
    output :: a -> String

instance Output Int where
    output = show

instance Output Integer where
    output = show

instance Output Double where
    output = show

instance Output Char where
    output c = show [c]

instance Output String where
    output = show

instance Output a => Output [a] where
    output = concat . intersperse " " . map output
