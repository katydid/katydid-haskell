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
import Control.Monad.Except (ExceptT, runExceptT, Except, throwError, runExcept)

import qualified Derive
import qualified Patterns
import Patterns (Refs, Pattern)
import IfExprs
import Expr
import Zip
import Parsers

mem :: Ord k => (k -> v) -> k -> M.Map k v -> (v, M.Map k v)
mem f k m = if M.member k m
    then (m M.! k, m)
    else let
        res = f k
        in (res, M.insert k res m)

type Nullable = M.Map Pattern Bool
type Calls = M.Map [Pattern] IfExprs
type Returns = M.Map ([Pattern], [Bool]) [Pattern]

-- |
-- Mem is the object used to store memoized results of the nullable, calls and returns functions.
type Mem = (Nullable, Calls, Returns)

-- |
-- newMem creates a object used for memoization by the validate function.
-- Each grammar should create its own memoize object.
newMem :: Mem
newMem = (M.empty, M.empty, M.empty)

-- |
-- nullable returns whether a pattern is nullable and memoizes the results.
nullable :: Refs -> Pattern -> State Mem Bool
nullable refs k = state $ \(n, c, r) -> let (v', n') = mem (Patterns.nullable refs) k n;
    in (v', (n', c, r))

calls :: Refs -> [Pattern] -> State Mem IfExprs
calls refs k = state $ \(n, c, r) -> let (v', c') = mem (Derive.calls refs) k c;
    in (v', (n, c', r))

returns :: Refs -> ([Pattern], [Bool]) -> State Mem [Pattern]
returns refs k = state $ \(n, c, r) -> let (v', r') = mem (Derive.returns refs) k r;
    in (v', (n, c, r'))

mderive :: Tree t => Refs -> [Pattern] -> [t] -> ExceptT ValueErr (State Mem) [Pattern]
mderive _ ps [] = return ps
mderive refs ps (tree:ts) = do {
    ifs <- lift $ calls refs ps;
    childps <- case runExcept $ evalIfExprs ifs (getLabel tree) of
        (Left l) -> throwError l
        (Right r) -> return r
    ;
    (zchildps, zipper) <- return $ zippy childps;
    childres <- mderive refs zchildps (getChildren tree);
    nulls <- lift $ mapM (nullable refs) childres;
    let unzipns = unzipby zipper nulls
    ;
    rs <- lift $ returns refs (ps, unzipns);
    mderive refs rs ts
}

-- |
-- derive is the classic derivative implementation for trees.
derive :: Tree t => Refs -> [t] -> Except String Pattern
derive refs ts =
    let start = [Patterns.lookupRef refs "main"]
        (res, _) = runState (runExceptT $ mderive refs start ts) newMem
    in case res of
        (Left l) -> throwError $ show l
        (Right [r]) -> return r
        (Right rs) -> throwError $ "not a single pattern: " ++ show rs

-- |
-- validate is the uses the derivative implementation for trees and
-- return whether tree is valid, given the input grammar and start pattern.
validate :: Tree t => Refs -> Pattern -> [t] -> (State Mem) Bool
validate refs start tree = do {
        rs <- runExceptT (mderive refs [start] tree);
        case rs of
        (Right [r]) -> nullable refs r
        _ -> return False
    }