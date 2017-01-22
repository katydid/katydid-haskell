module VpaDeriv where

import qualified Data.Map.Strict as DataMap
import Control.Monad.State
import Data.Foldable (foldlM)
import Debug.Trace

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

type VpaState = [Pattern]
type StackElm = ([Pattern], Zipper)

type DerivCallsM = DataMap.Map VpaState ZippedIfExprs
type NullableM = DataMap.Map [Pattern] [Bool]
type DerivReturnsM = DataMap.Map ([Pattern], Zipper, [Bool]) [Pattern]

type Vpa = (NullableM, DerivCallsM, DerivReturnsM, Refs)

newVpa :: Refs -> Vpa
newVpa refs = (DataMap.empty, DataMap.empty, DataMap.empty, refs)

mnullable :: [Pattern] -> State Vpa [Bool]
mnullable key = state $ \(n, c, r, refs) -> let (v', n') = mem (map $ nullable refs) key n;
    in (v', (n', c, r, refs))

mderivCalls :: [Pattern] -> State Vpa ZippedIfExprs
mderivCalls key = state $ \(n, c, r, refs) -> let (v', c') = mem (zipIfExprs . derivCalls refs) key c;
    in (v', (n, c', r, refs))

vpacall :: VpaState -> Label -> State Vpa (StackElm, VpaState)
vpacall vpastate label = do {
    zifexprs <- mderivCalls vpastate;
    (nextstate, zipper) <- return $ must $ evalZippedIfExprs zifexprs label;
    stackelm <- return $ (vpastate, zipper);
    return (stackelm, nextstate)
}

mderivReturns :: ([Pattern], Zipper, [Bool]) -> State Vpa [Pattern]
mderivReturns key = state $ \(n, c, r, refs) -> let (v', r') = mem (\(ps, zipper, znulls) -> derivReturns refs (ps, unzipby zipper znulls)) key r;
    in (v', (n, c, r', refs))

vpareturn :: StackElm -> VpaState -> State Vpa VpaState
vpareturn (vpastate, zipper) current = do {
    zipnulls <- mnullable current;
    mderivReturns (vpastate, zipper, zipnulls)
}

vpaderiv :: Tree t => VpaState -> t -> State Vpa VpaState
vpaderiv current tree = do {
    (stackelm, nextstate) <- vpacall current (getLabel tree);
    resstate <- foldlM vpaderiv nextstate (getChildren tree);
    vpareturn stackelm resstate
}

vderivs :: Tree t => Refs -> [t] -> Pattern
vderivs refs ts = case runState (foldlM vpaderiv [lookupRef refs "main"] ts) (newVpa refs) of
    ([r], (ns, cs, rs, refs)) -> trace (show $ length ns) r
    (rs, s) -> error $ "Number of patterns is not one, but " ++ show rs

