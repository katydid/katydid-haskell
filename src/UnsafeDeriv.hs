module UnsafeDeriv where

import Data.Foldable (foldlM)

import UnsafeMem
import Deriv
import IfExprs
import Patterns
import Values
import Parsers
import Simplify

data Mem = Mem {
	calls :: [Pattern] -> [IfExpr]
	, returns :: ([Pattern], [Bool]) -> [Pattern]
	, nullables :: Pattern -> Bool
	, simplifies :: Pattern -> Pattern
}

newMem :: Refs -> Mem
newMem refs = Mem (memoize (derivCalls refs)) (memoize (derivReturns refs)) (memoize (nullable refs)) (memoize (simplify refs))

uderivs :: Tree t => Refs -> [t] -> Value Pattern
uderivs g ts = let m = newMem g 
	in case foldlM (uderiv m) [lookupRef g "main"] ts of
    (Value [r]) -> Value r
    (Err e) -> Err e
    (Value rs) -> error $ "Number of patterns is not one, but " ++ show rs

uderiv :: Tree t => Mem -> [Pattern] -> t -> Value [Pattern]
uderiv mem ps tree =
    if all unescapable ps then Value ps else
    let ifs = (calls mem) ps
        simps = map (simplifies mem)
        d = uderiv mem
        nulls = map (nullables mem)
        rets = returns mem
    in do {
        childps <- mapM (evalIf (getLabel tree)) ifs;
        childres <- foldlM d (simps childps) (getChildren tree);
        return $ simps $ rets (ps, (nulls childres));
    }