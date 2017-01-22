module VpaDeriv where

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
type DerivCallsM = DataMap.Map [Pattern] ZippedIfExprs
type DerivReturnsM = DataMap.Map ([Pattern], [Bool]) [Pattern]

type Vpa = (NullableM, DerivCallsM, DerivReturnsM)

newVpa :: Vpa
newVpa = (DataMap.empty, DataMap.empty, DataMap.empty)

vnullable :: Refs -> Pattern -> State Vpa Bool
vnullable refs k = state $ \(n, c, r) -> let (v', n') = mem (nullable refs) k n;
    in (v', (n', c, r))

vderivCalls :: Refs -> [Pattern] -> State Vpa ZippedIfExprs
vderivCalls refs k = state $ \(n, c, r) -> let (v', c') = mem (zipIfExprs . derivCalls refs) k c;
    in (v', (n, c', r))

vderivReturns :: Refs -> ([Pattern], [Bool]) -> State Vpa [Pattern]
vderivReturns refs k = state $ \(n, c, r) -> let (v', r') = mem (derivReturns refs) k r;
    in (v', (n, c, r'))

vderiv :: Tree t => Refs -> [Pattern] -> t -> State Vpa [Pattern]
vderiv refs ps tree = do {
    ifs <- vderivCalls refs ps;
    (zchildps, zipper) <- return $ must $ evalZippedIfExprs ifs (getLabel tree);
    childres <- foldlM (vderiv refs) zchildps (getChildren tree);
    nulls <- mapM (vnullable refs) childres;
    unzipns <- return $ unzipby zipper nulls;
    vderivReturns refs (ps, unzipns)
}

vderivs :: Tree t => Refs -> [t] -> Pattern
vderivs refs ts = case evalState (foldlM (vderiv refs) [lookupRef refs "main"] ts) newVpa of
    [r] -> r
    rs -> error $ "Number of patterns is not one, but " ++ show rs

