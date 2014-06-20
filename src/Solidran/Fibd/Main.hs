import Data.Vector (Vector, (!), (!?))
import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vector

data Step
    = Step
        { total     :: Integer
        , newborn   :: Integer }
    deriving Show

calcNext :: Int -> Vector Step -> Step
calcNext m vec =
    let n = Vector.length vec
        new = newborn (vec ! (n - 1))
        old = total (vec ! (n - 1)) - newborn (vec ! (n - 1))
        die = newborn $ fromMaybe (Step 0 0) (vec !? (n - m))
    in case n of
        0 -> Step 1 1
        _ -> Step (new + old * 2 - die) old

sequenceTable :: Int -> Int -> Vector Step
sequenceTable n m =
    Vector.constructN n (calcNext m)

countRabbits :: Int -> Int -> Integer
countRabbits n m = total . Vector.last $ sequenceTable n m

main :: IO ()
main = do
    ln <- getLine
    let [n, m] = map read $ words ln
    print $ countRabbits n m
