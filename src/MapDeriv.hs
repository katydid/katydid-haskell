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

-- data Mem = Mem {
--       calls     :: MemCalls
--       , returns   :: MemReturns
--       , nullables :: MemNullable
--       , refs :: Refs
-- }

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

mustEvalIf :: IfExprs -> Label -> [Pattern]
mustEvalIf ifs l = case evalIfExprs l ifs of
    (Err e) -> error e
    (Value v) -> v

mderiv :: Tree t => Refs -> Mem -> [Pattern] -> t -> [Pattern]
mderiv refs m ps tree =
    if all unescapable ps then ps else
    let {
        ifs = derivCalls refs ps;
        simps = map (simplify refs);
        d = mderiv refs m;
        nulls = map (nullable refs);
        childps = mustEvalIf ifs (getLabel tree);
        (zchildps, zipper) = zippy (simps childps);
        childres = foldl d zchildps (getChildren tree);
        unzipns = unzipby zipper (nulls childres);
    } in simps $ derivReturns refs (ps, unzipns);

mderivs :: Tree t => Refs -> [t] -> Value Pattern
mderivs g ts = case foldl (mderiv g newMem) [lookupRef g "main"] ts of
    [r] -> Value r
    rs -> error $ "Number of patterns is not one, but " ++ show rs

-- mderivCalls :: Refs -> [Pattern] -> State (DataMap.Map [Pattern] IfExprs) IfExprs
-- mderivCalls refs ps = mem (derivCalls refs) ps

-- mderivReturns :: Refs -> ([Pattern], [Bool]) -> State (DataMap.Map ([Pattern], [Bool]) [Pattern]) [Pattern]
-- mderivReturns refs = mem (derivReturns refs)

-- mnullables :: Refs -> MemNullable -> [Pattern] -> (MemNullable, [Bool])
-- mnullables refs m [] = (m, [])
-- mnullables refs m (p:ps) = 
--     let (m', n) = mnullable refs m p
--         (m'', ns) = mnullables refs m' ps
--     in  (m'', n:ns)

-- getValue :: Ord k => (k -> Value v) -> DataMap.Map k v -> k -> (DataMap.Map k v, Value v)
-- getValue f m k = if DataMap.member k m
--     then (m, Value $ m DataMap.! k)
--     else case f k of
--         vv@(Value v) -> (DataMap.insert k v m, vv)
--         e@(Err _) -> (m, e)

-- mderiv :: Tree t => Refs -> (Mem, [Pattern]) -> t -> (Mem, Value [Pattern])
-- mderiv refs (m, patterns) tree =
--     if all unescapable patterns then (m, Value patterns) else
--     let (mcalls', ifs) = mderivCalls refs (calls m) patterns
--         m' = m {calls = mcalls'}
--         childps = map (evalIf (getLabel tree)) ifs
--         in case foldl (mderiv refs) (m', childps) (getChildren tree) of
--             (Err e) -> Err e
--             (Value (m'', childres)) -> 
--                 let (mnulls, childns) = mnullables refs (nullables m'') childres
--                     m''' = m { nullables = mnulls }
--                     (mreturns, res) = mderivReturns refs ( returns m ) (patterns, childns)
--                     m'''' = m { returns = mreturns }
--                 in (m'''', res)
