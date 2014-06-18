module Solidran.Iprb.Detail 
    ( OrgType (..)
    , Env
    , Prob
    , totalProb
    ) where

import Control.Monad.State (State)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Control.Monad.State as State
import qualified Data.Map as Map

data OrgType 
    = HomoDom
    | Hetero
    | HomoRec
    deriving (Eq, Ord)

type Env    = Map OrgType Int
type Prob   = Double

probDom :: OrgType -> OrgType -> Prob
probDom Hetero Hetero   = 3 / 4
probDom Hetero HomoRec  = 2 / 4
probDom HomoRec Hetero  = 2 / 4
probDom HomoRec HomoRec = 0.0
probDom _       _       = 1.0

prob :: OrgType -> State Env Prob
prob o = do
    m <- State.get
    let no = fromMaybe 0 $ Map.lookup o m
    let tot = Map.foldr (+) 0 m
    if tot /= 0
        then return $ fromIntegral no / fromIntegral tot
        else return 0

draw :: OrgType -> State Env ()
draw o = do
    State.modify $ Map.adjust (subtract 1) o

branch :: OrgType -> OrgType -> State Env Prob
branch x y = do
    px <- prob x
    draw x
    py <- prob y
    draw y -- not needed
    let pd = probDom x y
    return $ px * py * pd

orgList :: [OrgType]
orgList = [HomoDom, Hetero, HomoRec]

totalProb :: Env -> Prob
totalProb e = sum [State.evalState (branch x y) e | x <- orgList, y <- orgList]
