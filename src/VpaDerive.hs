-- |
-- This module contains a VPA (Visual Pushdown Automaton) implementation of the internal derivative algorithm.
--
-- It is intended to be used for explanation purposes.
--
-- It shows how out algorithm is effective equivalent to a visual pushdown automaton.

module VpaDerive (
    derive      
) where

import qualified Data.Map.Strict as DataMap
import Control.Monad.State (State, runState, state, lift)
import Data.Foldable (foldlM)
import Control.Monad.Except (Except, ExceptT, throwError, runExcept, runExceptT)

import qualified Derive
import Patterns (Refs, Pattern)
import qualified Patterns
import IfExprs
import Expr
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

type Calls = DataMap.Map VpaState ZippedIfExprs
type Nullable = DataMap.Map [Pattern] [Bool]
type Returns = DataMap.Map ([Pattern], Zipper, [Bool]) [Pattern]

type Vpa = (Nullable, Calls, Returns, Refs)

newVpa :: Refs -> Vpa
newVpa refs = (DataMap.empty, DataMap.empty, DataMap.empty, refs)

nullable :: [Pattern] -> State Vpa [Bool]
nullable key = state $ \(n, c, r, refs) -> let (v', n') = mem (map $ Patterns.nullable refs) key n;
    in (v', (n', c, r, refs))

calls :: [Pattern] -> State Vpa ZippedIfExprs
calls key = state $ \(n, c, r, refs) -> let (v', c') = mem (zipIfExprs . Derive.calls refs) key c;
    in (v', (n, c', r, refs))

vpacall :: VpaState -> Label -> ExceptT ValueErr (State Vpa) (StackElm, VpaState)
vpacall vpastate label = do {
    zifexprs <- lift $ calls vpastate;
    (nextstate, zipper) <- case runExcept $ evalZippedIfExprs zifexprs label of
        (Left l) -> throwError l
        (Right r) -> return r
    ;
    let stackelm = (vpastate, zipper)
    ; 
    return (stackelm, nextstate)
}

returns :: ([Pattern], Zipper, [Bool]) -> State Vpa [Pattern]
returns key = state $ \(n, c, r, refs) -> 
    let (v', r') = mem (\(ps, zipper, znulls) -> 
            Derive.returns refs (ps, unzipby zipper znulls)) key r
    in (v', (n, c, r', refs))

vpareturn :: StackElm -> VpaState -> State Vpa VpaState
vpareturn (vpastate, zipper) current = do {
    zipnulls <- nullable current;
    returns (vpastate, zipper, zipnulls)
}

deriv :: Tree t => VpaState -> t -> ExceptT ValueErr (State Vpa) VpaState
deriv current tree = do {
    (stackelm, nextstate) <- vpacall current (getLabel tree);
    resstate <- foldlM deriv nextstate (getChildren tree);
    lift $ vpareturn stackelm resstate
}

foldLT :: Tree t => Vpa -> VpaState -> [t] -> Except ValueErr [Pattern]
foldLT _ current [] = return current
foldLT m current (t:ts) = 
    let (newstate, newm) = runState (runExceptT $ deriv current t) m
    in case newstate of
        (Left l) -> throwError l
        (Right r) -> foldLT newm r ts

derive :: Tree t => Refs -> [t] -> Except String Pattern
derive refs ts = 
    let start = [Patterns.lookupRef refs "main"]
    in case runExcept $ foldLT (newVpa refs) start ts of
        (Left l) -> throwError $ show l
        (Right [r]) -> return r
        (Right rs) -> throwError $ "Number of patterns is not one, but " ++ show rs
