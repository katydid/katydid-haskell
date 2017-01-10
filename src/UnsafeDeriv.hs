module UnsafeDeriv where

import UnsafeMem
import Deriv
import IfExprs
import Patterns
import Values
import Parsers

data Mem = Mem {
	calls :: [Pattern] -> [IfExpr]
	, returns :: ([Pattern], [Bool]) -> [Pattern]
	, nullables :: Pattern -> Bool
}

newMem :: Refs -> Mem
newMem refs = Mem (memoize (derivCalls refs)) (memoize (derivReturns refs)) (memoize (nullable refs))

-- uderiv :: Tree t => Mem -> [Pattern] -> t -> [Pattern]
-- uderiv mem ps tree =
-- 	if all unescapable ps then ps else
-- 	let	ifs = (calls mem) ps
-- 		childps = map (evalIf (getLabel tree)) ifs
-- 		childres = foldl (uderiv mem) childps (getChildren tree)
-- 		childns = map (nullables mem) childres
-- 	in (returns mem) (ps, childns)