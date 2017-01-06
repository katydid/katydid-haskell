module UnsafeDeriv where

import qualified Data.Map.Strict as Map
import qualified Data.Tree as Tree
import UnsafeMem
import Deriv
import IfExprs
import Patterns
import Values
import ParsedTree

data Mem = Mem {
	calls :: [Pattern] -> [IfExpr]
	, returns :: ([Pattern], [Bool]) -> [Pattern]
	, nullables :: Pattern -> Bool
}

newMem :: Refs -> Mem
newMem refs = Mem (memoize (derivCalls refs)) (memoize (derivReturns refs)) (memoize (nullable refs))

uderiv :: Mem -> [Pattern] -> Tree.Tree MyLabel -> [Pattern]
uderiv mem ps (Tree.Node label children) =
	if all unescapable ps then ps else
	let	ifs = (calls mem) ps
		childps = map (evalIf label) ifs
		childres = foldl (uderiv mem) childps children
		childns = map (nullables mem) childres
	in (returns mem) (ps, childns)