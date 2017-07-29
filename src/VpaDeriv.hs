module VpaDeriv (
    vderivs      
) where

import qualified Data.Map.Strict as DataMap
import Control.Monad.State (State, runState, state, lift)
import Data.Foldable (foldlM)
import Control.Monad.Except (Except, ExceptT, throwError, runExcept, runExceptT)

import Deriv
import Patterns
import IfExprs
import Values
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

vpacall :: VpaState -> Label -> ExceptT ValueErr (State Vpa) (StackElm, VpaState)
vpacall vpastate label = do {
    zifexprs <- lift $ mderivCalls vpastate;
    (nextstate, zipper) <- case runExcept $ evalZippedIfExprs zifexprs label of
        (Left l) -> throwError l
        (Right r) -> return r
    ;
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

vpaderiv :: Tree t => VpaState -> t -> ExceptT ValueErr (State Vpa) VpaState
vpaderiv current tree = do {
    (stackelm, nextstate) <- vpacall current (getLabel tree);
    resstate <- foldlM vpaderiv nextstate (getChildren tree);
    lift $ vpareturn stackelm resstate
}

foldLT :: Tree t => Vpa -> VpaState -> [t] -> Except ValueErr [Pattern]
foldLT _ current [] = return current
foldLT m current (t:ts) = 
    let (newstate, newm) = runState (runExceptT $ vpaderiv current t) m
    in case newstate of
        (Left l) -> throwError l
        (Right r) -> foldLT newm r ts

vderivs :: Tree t => Refs -> [t] -> Except String Pattern
vderivs refs ts = 
    let start = [lookupRef refs "main"]
    in case runExcept $ foldLT (newVpa refs) start ts of
        (Left l) -> throwError $ show l
        (Right [r]) -> return r
        (Right rs) -> throwError $ "Number of patterns is not one, but " ++ show rs
