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

type Vpa = (NullableM, DerivCallsM, DerivReturnsM)

newVpa :: Vpa
newVpa = (DataMap.empty, DataMap.empty, DataMap.empty)

mnullable :: Refs -> [Pattern] -> State Vpa [Bool]
mnullable refs key = state $ \(n, c, r) -> let (v', n') = mem (map $ nullable refs) key n;
    in (v', (n', c, r))

mderivCalls :: Refs -> [Pattern] -> State Vpa ZippedIfExprs
mderivCalls refs key = state $ \(n, c, r) -> let (v', c') = mem (zipIfExprs . derivCalls refs) key c;
    in (v', (n, c', r))

vpacall :: Refs -> VpaState -> Label -> State Vpa (StackElm, VpaState)
vpacall refs vpastate label = do {
    zifexprs <- mderivCalls refs vpastate;
    (nextstate, zipper) <- return $ must $ evalZippedIfExprs zifexprs label;
    stackelm <- return $ (vpastate, zipper);
    return (stackelm, nextstate)
}

mderivReturns :: Refs -> ([Pattern], Zipper, [Bool]) -> State Vpa [Pattern]
mderivReturns refs key = state $ \(n, c, r) -> let (v', r') = mem (\(ps, zipper, znulls) -> derivReturns refs (ps, unzipby zipper znulls)) key r;
    in (v', (n, c, r'))

vpareturn :: Refs -> StackElm -> VpaState -> State Vpa VpaState
vpareturn refs (vpastate, zipper) current = do {
    zipnulls <- mnullable refs current;
    mderivReturns refs (vpastate, zipper, zipnulls)
}

vpaderiv :: Tree t => Refs -> VpaState -> t -> State Vpa VpaState
vpaderiv refs current tree = do {
    (stackelm, nextstate) <- vpacall refs current (getLabel tree);
    resstate <- foldlM (vpaderiv refs) nextstate (getChildren tree);
    vpareturn refs stackelm resstate
}

vderivs :: Tree t => Refs -> [t] -> Pattern
vderivs refs ts = case runState (foldlM (vpaderiv refs) [lookupRef refs "main"] ts) newVpa of
    ([r], (ns, cs, rs)) -> trace (show $ length ns) r
    (rs, s) -> error $ "Number of patterns is not one, but " ++ show rs

