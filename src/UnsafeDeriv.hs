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
    calls :: [Pattern] -> IfExprs
    , returns :: ([Pattern], [Bool]) -> [Pattern]
    , nullables :: Pattern -> Bool
    , simplifies :: Pattern -> Pattern
}

newMem :: Refs -> Mem
newMem refs =
    let simp = simplify refs
        simps = map simp
        calls = derivCalls refs
        returns = simps . (derivReturns refs)
    in Mem (memoize calls) (memoize returns) (memoize (nullable refs)) simp

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
        d = uderiv mem
        nulls = map (nullables mem)
        rets = returns mem
    in do {
        childps <- evalIfExprs (getLabel tree) ifs;
        childres <- foldlM d childps (getChildren tree);
        return $ rets (ps, (nulls childres));
    }