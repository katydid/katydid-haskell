-- |
-- This module contains a VPA (Visibly Pushdown Automaton) implementation of the internal derivative algorithm.
--
-- It is intended to be used for explanation purposes.
--
-- It shows how our algorithm is effectively equivalent to a visibly pushdown automaton.

module Data.Katydid.Relapse.VpaDerive (
    derive      
) where

import qualified Data.Map.Strict as M
import Control.Monad.State (State, runState, state, lift)
import Data.Foldable (foldlM)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)

import Data.Katydid.Parser.Parser

import qualified Data.Katydid.Relapse.Derive as Derive
import Data.Katydid.Relapse.Smart (Grammar, Pattern)
import qualified Data.Katydid.Relapse.Smart as Smart
import Data.Katydid.Relapse.IfExprs
import Data.Katydid.Relapse.Expr
import Data.Katydid.Relapse.Zip

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

newtype Vpa = Vpa (Nullable, Calls, Returns, Grammar)

newVpa :: Grammar -> Vpa
newVpa g = Vpa (M.empty, M.empty, M.empty, g)

nullable :: [Pattern] -> State Vpa [Bool]
nullable key = state $ \(Vpa (n, c, r, g)) -> let (v', n') = mem (map Smart.nullable) key n;
    in (v', Vpa (n', c, r, g))

calls :: [Pattern] -> State Vpa ZippedIfExprs
calls key = state $ \(Vpa (n, c, r, g)) -> let (v', c') = mem (zipIfExprs . Derive.calls g) key c;
    in (v', Vpa (n, c', r, g))

vpacall :: VpaState -> Label -> ExceptT String (State Vpa) (StackElm, VpaState)
vpacall vpastate label = do {
    zifexprs <- lift $ calls vpastate;
    (nextstate, zipper) <- hoistExcept $ evalZippedIfExprs zifexprs label;
    let 
        stackelm = (vpastate, zipper)
    ; 
    return (stackelm, nextstate)
}

hoistExcept :: (Monad m) => Either e a -> ExceptT e m a
hoistExcept = ExceptT . return

returns :: ([Pattern], Zipper, [Bool]) -> State Vpa [Pattern]
returns key = state $ \(Vpa (n, c, r, g)) -> 
    let (v', r') = mem (\(ps, zipper, znulls) -> 
            Derive.returns g (ps, unzipby zipper znulls)) key r
    in (v', Vpa (n, c, r', g))

vpareturn :: StackElm -> VpaState -> State Vpa VpaState
vpareturn (vpastate, zipper) current = do {
    zipnulls <- nullable current;
    returns (vpastate, zipper, zipnulls)
}

deriv :: Tree t => VpaState -> t -> ExceptT String (State Vpa) VpaState
deriv current tree = do {
    (stackelm, nextstate) <- vpacall current (getLabel tree);
    resstate <- foldlM deriv nextstate (getChildren tree);
    lift $ vpareturn stackelm resstate
}

foldLT :: Tree t => Vpa -> VpaState -> [t] -> Either String [Pattern]
foldLT _ current [] = return current
foldLT m current (t:ts) = 
    let (newstate, newm) = runState (runExceptT $ deriv current t) m
    in case newstate of
        (Left l) -> Left l
        (Right r) -> foldLT newm r ts

-- |
-- derive is the derivative implementation for trees.
-- This implementation makes use of visual pushdown automata.
derive :: Tree t => Grammar -> [t] -> Either String Pattern
derive g ts = 
    let start = [Smart.lookupMain g]
    in case foldLT (newVpa g) start ts of
        (Left l) -> Left $ show l
        (Right [r]) -> return r
        (Right rs) -> Left $ "Number of patterns is not one, but " ++ show rs
