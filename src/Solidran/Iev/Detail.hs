module Solidran.Iev.Detail 
    ( expectedDom
    ) where

data GenoType
    = Hetero
    | HomoRec
    | HomoDom
    deriving (Eq, Show)

type Prob = Double

probDom :: GenoType -> GenoType -> Prob
probDom Hetero Hetero   = 0.75
probDom HomoRec Hetero  = 0.5
probDom Hetero HomoRec  = 0.5
probDom HomoRec HomoRec = 0.0
probDom _       _       = 1.0

expectedDom :: (Int, Int, Int, Int, Int, Int) -> Double
expectedDom (a, b, c, d, e, f) =
    sum
        [ probDom HomoDom HomoDom * fromIntegral a * 2
        , probDom HomoDom Hetero  * fromIntegral b * 2
        , probDom HomoDom HomoRec * fromIntegral c * 2
        , probDom Hetero Hetero   * fromIntegral d * 2
        , probDom Hetero HomoRec  * fromIntegral e * 2
        , probDom HomoRec HomoRec * fromIntegral f * 2 ]
