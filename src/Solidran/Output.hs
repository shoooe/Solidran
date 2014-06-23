{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Solidran.Output 
    ( output
    , Str(..)
    ) where

import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as Map

newtype Str
    = Str String
    deriving (Eq, Ord, Show)

class Show a => Output a where
    output :: a -> String
    output = show

instance Output Int
instance Output Integer
instance Output Double
instance Output Float

instance Output Str where
    output (Str s) = s

instance Output Char where
    output c = [c]

instance Output a => Output [a] where
    output = concat . intersperse " " . map output

instance (Output k, Output v) => Output (Map k v) where
    output = Map.foldrWithKey fn ""
        where fn k v c = concat [output k, ": ", output v, "\n", c]

instance (Output v) => Output (Map String v) where
    output = Map.foldrWithKey fn ""
        where fn k v c = concat [k, ": ", output v, "\n", c]

instance (Output a, Output b) => Output (a, b) where
    output (a, b) = concat [output a, " ", output b]
