-- |
-- This module contains a VPA (Visual Pushdown Automaton) implementation of the internal derivative algorithm.
--
-- It is intended to be used for explanation purposes.
--
-- It shows how out algorithm is effective equivalent to a visual pushdown automaton.

module VpaDerive (
    derive      
) where

import qualified Data.Map.Strict as M
import Control.Monad.State (State, runState, state, lift)
import Data.Foldable (foldlM)
import Control.Monad.Trans.Either (EitherT, runEitherT, left, hoistEither)

import qualified Derive
import Patterns (Refs, Pattern)
import qualified Patterns
import IfExprs
import Expr
import Zip
import Parsers

mem :: Ord k => (k -> v) -> k -> M.Map k v -> (v, M.Map k v)
mem f k m
    | M.member k m = (m M.! k, m)
    | otherwise = let res = f k
        in (res, M.insert k res m)

type VpaState = [Pattern]
type StackElm = ([Pattern], Zipper)

type Calls = M.Map VpaState ZippedIfExprs
type Nullable = M.Map [Pattern] [Bool]
type Returns = M.Map ([Pattern], Zipper, [Bool]) [Pattern]

newtype Vpa = Vpa (Nullable, Calls, Returns, Refs)

newVpa :: Refs -> Vpa
newVpa refs = Vpa (M.empty, M.empty, M.empty, refs)

nullable :: [Pattern] -> State Vpa [Bool]
nullable key = state $ \(Vpa (n, c, r, refs)) -> let (v', n') = mem (map $ Patterns.nullable refs) key n;
    in (v', Vpa (n', c, r, refs))

calls :: [Pattern] -> State Vpa ZippedIfExprs
calls key = state $ \(Vpa (n, c, r, refs)) -> let (v', c') = mem (zipIfExprs . Derive.calls refs) key c;
    in (v', Vpa (n, c', r, refs))

vpacall :: VpaState -> Label -> EitherT String (State Vpa) (StackElm, VpaState)
vpacall vpastate label = do {
    zifexprs <- lift $ calls vpastate;
    (nextstate, zipper) <- hoistEither $ evalZippedIfExprs zifexprs label;
    let 
        stackelm = (vpastate, zipper)
    ; 
    return (stackelm, nextstate)
}

returns :: ([Pattern], Zipper, [Bool]) -> State Vpa [Pattern]
returns key = state $ \(Vpa (n, c, r, refs)) -> 
    let (v', r') = mem (\(ps, zipper, znulls) -> 
            Derive.returns refs (ps, unzipby zipper znulls)) key r
    in (v', Vpa (n, c, r', refs))

vpareturn :: StackElm -> VpaState -> State Vpa VpaState
vpareturn (vpastate, zipper) current = do {
    zipnulls <- nullable current;
    returns (vpastate, zipper, zipnulls)
}

deriv :: Tree t => VpaState -> t -> EitherT String (State Vpa) VpaState
deriv current tree = do {
    (stackelm, nextstate) <- vpacall current (getLabel tree);
    resstate <- foldlM deriv nextstate (getChildren tree);
    lift $ vpareturn stackelm resstate
}

foldLT :: Tree t => Vpa -> VpaState -> [t] -> Either String [Pattern]
foldLT _ current [] = return current
foldLT m current (t:ts) = 
    let (newstate, newm) = runState (runEitherT $ deriv current t) m
    in case newstate of
        (Left l) -> Left l
        (Right r) -> foldLT newm r ts

-- |
-- derive is the derivative implementation for trees.
-- This implementation makes use of visual pushdown automata.
derive :: Tree t => Refs -> [t] -> Either String Pattern
derive refs ts = 
    let start = [Patterns.lookupRef refs "main"]
    in case foldLT (newVpa refs) start ts of
        (Left l) -> Left $ show l
        (Right [r]) -> return r
        (Right rs) -> Left $ "Number of patterns is not one, but " ++ show rs
