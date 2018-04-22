-- |
-- This module is an efficient implementation of the derivative algorithm for trees.
--
-- It is intended to be used for production purposes.
--
-- This means that it gives up some readability for speed.
--
-- This module provides memoization of the nullable, calls and returns functions.

module MemDerive (
    derive, Mem, newMem, nullable, validate
) where

import qualified Data.Map.Strict as M
import Control.Monad.State (State, runState, lift, state)
import Control.Monad.Trans.Either (EitherT, runEitherT, left, hoistEither)

import qualified Derive
import qualified Ast
import Ast (Grammar, Pattern)
import IfExprs
import Expr
import Zip
import Parsers

mem :: Ord k => (k -> v) -> k -> M.Map k v -> (v, M.Map k v)
mem f k m
    | M.member k m = (m M.! k, m)
    | otherwise = let res = f k
        in (res, M.insert k res m)

type Nullable = M.Map Pattern Bool
type Calls = M.Map [Pattern] IfExprs
type Returns = M.Map ([Pattern], [Bool]) [Pattern]

-- |
-- Mem is the object used to store memoized results of the nullable, calls and returns functions.
newtype Mem = Mem (Nullable, Calls, Returns)

-- |
-- newMem creates a object used for memoization by the validate function.
-- Each grammar should create its own memoize object.
newMem :: Mem
newMem = Mem (M.empty, M.empty, M.empty)

-- |
-- nullable returns whether a pattern is nullable and memoizes the results.
nullable :: Grammar -> Pattern -> State Mem Bool
nullable g k = state $ \(Mem (n, c, r)) -> let (v', n') = mem (Ast.nullable g) k n;
    in (v', Mem (n', c, r))

calls :: Grammar -> [Pattern] -> State Mem IfExprs
calls g k = state $ \(Mem (n, c, r)) -> let (v', c') = mem (Derive.calls g) k c;
    in (v', Mem (n, c', r))

returns :: Grammar -> ([Pattern], [Bool]) -> State Mem [Pattern]
returns g k = state $ \(Mem (n, c, r)) -> let (v', r') = mem (Derive.returns g) k r;
    in (v', Mem (n, c, r'))

mderive :: Tree t => Grammar -> [Pattern] -> [t] -> EitherT String (State Mem) [Pattern]
mderive _ ps [] = return ps
mderive g ps (tree:ts) = do {
    ifs <- lift $ calls g ps;
    childps <- hoistEither $ evalIfExprs ifs (getLabel tree);
    (zchildps, zipper) <- return $ zippy childps;
    childres <- mderive g zchildps (getChildren tree);
    nulls <- lift $ mapM (nullable g) childres;
    let 
        unzipns = unzipby zipper nulls
    ;
    rs <- lift $ returns g (ps, unzipns);
    mderive g rs ts
}

-- |
-- derive is the classic derivative implementation for trees.
derive :: Tree t => Grammar -> [t] -> Either String Pattern
derive g ts =
    let start = [Ast.lookupRef g "main"]
        (res, _) = runState (runEitherT $ mderive g start ts) newMem
    in case res of
        (Left l) -> Left $ show l
        (Right [r]) -> return r
        (Right rs) -> Left $ "not a single pattern: " ++ show rs

-- |
-- validate is the uses the derivative implementation for trees and
-- return whether tree is valid, given the input grammar and start pattern.
validate :: Tree t => Grammar -> Pattern -> [t] -> (State Mem) Bool
validate g start tree = do {
        rs <- runEitherT (mderive g [start] tree);
        case rs of
        (Right [r]) -> nullable g r
        _ -> return False
    }