module MapDeriv where

import qualified Data.Map.Strict as DataMap

import Deriv
import Patterns
import IfExprs
import Values
import Parsers

data Mem = Mem {
	  calls     :: MemCalls
	  , returns   :: MemReturns
	  , nullables :: MemNullable
}

type MemNullable = DataMap.Map Pattern Bool

mnullable :: Refs -> MemNullable -> Pattern -> (MemNullable, Bool)
mnullable refs = get (nullable refs)

mnullables :: Refs -> MemNullable -> [Pattern] -> (MemNullable, [Bool])
mnullables refs m [] = (m, [])
mnullables refs m (p:ps) = 
	let	(m', n) = mnullable refs m p
		(m'', ns) = mnullables refs m' ps
	in 	(m'', n:ns)

mem :: Ord k => (k -> v) -> DataMap.Map k v -> k -> DataMap.Map k v
mem f m k = if DataMap.member k m
	then m
	else DataMap.insert k (f k) m

get :: Ord k => (k -> v) -> DataMap.Map k v -> k -> (DataMap.Map k v, v)
get f m k = (m', m' DataMap.! k)
	where m' = mem f m k

type MemCalls = DataMap.Map [Pattern] [IfExpr]

mderivCalls :: Refs -> MemCalls -> [Pattern] -> (MemCalls, [IfExpr])
mderivCalls refs = get (derivCalls refs)

type MemReturns = DataMap.Map ([Pattern], [Bool]) [Pattern]

mderivReturns :: Refs -> MemReturns -> ([Pattern], [Bool]) -> (MemReturns, [Pattern])
mderivReturns refs = get (derivReturns refs)

-- mderiv :: Tree t => Refs -> (Mem, [Pattern]) -> t -> (Mem, [Pattern])
-- mderiv refs (m, patterns) tree =
-- 	if all unescapable patterns then (m, patterns) else
-- 	let	(mcalls', ifs) = mderivCalls refs (calls m) patterns
-- 		m' = m {calls = mcalls'}
-- 		childps = map (evalIf (getLabel tree)) ifs
-- 		(m'', childres) = foldl (mderiv refs) (m', childps) (getChildren tree)
-- 		(mnulls, childns) = mnullables refs (nullables m'') childres
-- 		m''' = m { nullables = mnulls }
-- 		(mreturns, res) = mderivReturns refs ( returns m ) (patterns, childns)
-- 		m'''' = m { returns = mreturns }
-- 	in (m'''', res)