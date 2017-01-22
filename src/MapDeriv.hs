module MapDeriv where

import qualified Data.Map.Strict as DataMap
import Control.Monad.State
import Data.Foldable (foldlM)

import Deriv
import Patterns
import IfExprs
import Values
import Simplify
import Zip
import Parsers

mem :: Ord k => (k -> v) -> k -> DataMap.Map k v -> (v, DataMap.Map k v)
mem f k m = if DataMap.member k m
    then (m DataMap.! k, m)
    else let
        res = f k
        in (res, DataMap.insert k res m)

type NullableM = DataMap.Map Pattern Bool
type DerivCallsM = DataMap.Map [Pattern] IfExprs
type DerivReturnsM = DataMap.Map ([Pattern], [Bool]) [Pattern]

type Mem = (NullableM, DerivCallsM, DerivReturnsM)

newMem :: Mem
newMem = (DataMap.empty, DataMap.empty, DataMap.empty)

mnullable :: Refs -> Pattern -> State Mem Bool
mnullable refs k = state $ \(n, c, r) -> let (v', n') = mem (nullable refs) k n;
    in (v', (n', c, r))

mderivCalls :: Refs -> [Pattern] -> State Mem IfExprs
mderivCalls refs k = state $ \(n, c, r) -> let (v', c') = mem (derivCalls refs) k c;
    in (v', (n, c', r))

mderivReturns :: Refs -> ([Pattern], [Bool]) -> State Mem [Pattern]
mderivReturns refs k = state $ \(n, c, r) -> let (v', r') = mem (derivReturns refs) k r;
    in (v', (n, c, r'))

mderiv :: Tree t => Refs -> [Pattern] -> t -> State Mem [Pattern]
mderiv refs ps tree = do {
    ifs <- mderivCalls refs ps;
    childps <- return $ must $ evalIfExprs ifs (getLabel tree);
    (zchildps, zipper) <- return $ zippy childps;
    childres <- foldlM (mderiv refs) zchildps (getChildren tree);
    nulls <- mapM (mnullable refs) childres;
    unzipns <- return $ unzipby zipper nulls;
    mderivReturns refs (ps, unzipns)
}

mderivs :: Tree t => Refs -> [t] -> Value Pattern
mderivs refs ts = case evalState (foldlM (mderiv refs) [lookupRef refs "main"] ts) newMem of
    [r] -> Value r
    rs -> error $ "Number of patterns is not one, but " ++ show rs

