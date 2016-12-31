module MapDeriv where

import Deriv
import qualified Data.Map.Strict as Map
import qualified Data.Tree as Tree

data Mem = Mem {
	  calls     :: MemCalls
	  , returns   :: MemReturns
	  , nullables :: MemNullable
}

type MemNullable = Map.Map Pattern Bool

mnullable :: Refs -> MemNullable -> Pattern -> (MemNullable, Bool)
mnullable refs = get (nullable refs)

mnullables :: Refs -> MemNullable -> [Pattern] -> (MemNullable, [Bool])
mnullables refs m [] = (m, [])
mnullables refs m (p:ps) = 
	let	(m', n) = mnullable refs m p
		(m'', ns) = mnullables refs m' ps
	in 	(m'', n:ns)

mem :: Ord k => (k -> v) -> Map.Map k v -> k -> Map.Map k v
mem f m k = if Map.member k m
	then m
	else Map.insert k (f k) m

get :: Ord k => (k -> v) -> Map.Map k v -> k -> (Map.Map k v, v)
get f m k = (m', m' Map.! k)
	where m' = mem f m k

type MemCalls = Map.Map [Pattern] [IfExpr]

mderivCalls :: Refs -> MemCalls -> [Pattern] -> (MemCalls, [IfExpr])
mderivCalls refs = get (derivCalls refs)

type MemReturns = Map.Map ([Pattern], [Bool]) [Pattern]

mderivReturns :: Refs -> MemReturns -> ([Pattern], [Bool]) -> (MemReturns, [Pattern])
mderivReturns refs = get (derivReturns refs)

mderiv :: Refs -> (Mem, [Pattern]) -> Tree.Tree ValueType -> (Mem, [Pattern])
mderiv refs (m, patterns) (Tree.Node label children) =
	let	(mcalls', ifs) = mderivCalls refs (calls m) patterns
		m' = m {calls = mcalls'}
		childps = map (evalIf label) ifs
		(m'', childres) = foldl (mderiv refs) (m', childps) children
		(mnulls, childns) = mnullables refs (nullables m'') childres
		m''' = m { nullables = mnulls }
		(mreturns, res) = mderivReturns refs ( returns m ) (patterns, childns)
		m'''' = m { returns = mreturns }
	in (m'''', res)