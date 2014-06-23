module Solidran.Grph.Detail 
    ( adjacencyList 
    ) where

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Solidran.Fasta as Fasta

data Node
    = Node
        { label     :: String
        , leftEdge  :: String
        , rightEdge :: String }
    deriving (Show, Eq)

parseFasta :: Int -> Map String String -> [Node]
parseFasta i = Map.foldrWithKey parseNode []
    where parseNode k v c = 
              (Node k (take i v) (drop (length v - i) v)) : c

nodeMap :: [Node] -> Map String [Node]
nodeMap = foldr fn Map.empty
    where fn n m = Map.insertWith (++) (leftEdge n) [n] m

nodeAdj :: Map String [Node] -> [(Node, Node)]
nodeAdj m = Map.foldr (\ l c -> foldr fn c l) [] m
    where fn n c = c ++ map (\x -> (n, x)) (fromMaybe [] $ Map.lookup (rightEdge n) m)

adjacencyList :: Int -> String -> [(String, String)]
adjacencyList i = fe . map (\ (n1, n2) -> (label n1, label n2)) . na
    where na = nodeAdj . nodeMap . parseFasta i . Fasta.parse
          fe = filter (\(s1, s2) -> s1 /= s2) 
