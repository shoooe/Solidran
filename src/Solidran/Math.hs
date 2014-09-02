module Solidran.Math (factorial) where

factorial :: Int -> Integer
factorial n = product [1..toInteger n]
