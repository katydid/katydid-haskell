module MapDeriv (
    mderivs
) where

import qualified Data.Map.Strict as DataMap
import Control.Monad.State (State, runState, evalState, lift, state)
import Data.Foldable (foldlM)
import Control.Monad.Except (ExceptT, runExceptT, Except, throwError, runExcept)

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

mderiv :: Tree t => Refs -> [Pattern] -> t -> ExceptT ValueErr (State Mem) [Pattern]
mderiv refs ps tree = do {
    ifs <- lift $ mderivCalls refs ps;
    childps <- case runExcept $ evalIfExprs ifs (getLabel tree) of
        (Left l) -> throwError l
        (Right r) -> return r
    ;
    (zchildps, zipper) <- return $ zippy childps;
    childres <- foldlM (mderiv refs) zchildps (getChildren tree);
    nulls <- lift $ mapM (mnullable refs) childres;
    unzipns <- return $ unzipby zipper nulls;
    lift $ mderivReturns refs (ps, unzipns)
}

foldLT :: Tree t => Mem -> ([Pattern] -> t -> ExceptT ValueErr (State Mem) [Pattern]) -> [Pattern] -> [t] -> Except ValueErr [Pattern]
foldLT _ _ ps [] = return ps
foldLT m d ps (t:ts) = 
    let (newps, newm) = runState (runExceptT $ d ps t) m
    in case newps of
        (Left l) -> throwError l
        (Right r) -> foldLT newm d r ts

removeState :: Mem -> ExceptT ValueErr (State Mem) [Pattern] -> (Either ValueErr [Pattern])
removeState m e = evalState (runExceptT e) m

mderivs :: Tree t => Refs -> [t] -> Except String Pattern
mderivs refs ts =
    let start = [lookupRef refs "main"]
        f = mderiv refs
    in case runExcept $ foldLT newMem f start ts of
        (Left l) -> throwError $ show l
        (Right [r]) -> return r
        (Right rs) -> throwError $ "not a single pattern: " ++ show rs
