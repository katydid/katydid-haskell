module MemDerive (
    derive, Mem, newMem, nullable, validate
) where

import qualified Data.Map.Strict as DataMap
import Control.Monad.State (State, runState, lift, state)
import Control.Monad.Except (ExceptT, runExceptT, Except, throwError, runExcept)

import qualified Derive
import qualified Patterns
import Patterns (Refs, Pattern)
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

type Nullable = DataMap.Map Pattern Bool
type Calls = DataMap.Map [Pattern] IfExprs
type Returns = DataMap.Map ([Pattern], [Bool]) [Pattern]

type Mem = (Nullable, Calls, Returns)

newMem :: Mem
newMem = (DataMap.empty, DataMap.empty, DataMap.empty)

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
    unzipns <- return $ unzipby zipper nulls;
    rs <- lift $ returns refs (ps, unzipns);
    mderive refs rs ts
}

derive :: Tree t => Refs -> [t] -> Except String Pattern
derive refs ts =
    let start = [Patterns.lookupRef refs "main"]
        (res, _) = runState (runExceptT $ mderive refs start ts) newMem
    in case res of
        (Left l) -> throwError $ show l
        (Right [r]) -> return r
        (Right rs) -> throwError $ "not a single pattern: " ++ show rs

validate :: Tree t => Refs -> Pattern -> [t] -> (State Mem) Bool
validate refs start tree = do {
        rs <- runExceptT (mderive refs [start] tree);
        case rs of
        (Right [r]) -> nullable refs r
        _ -> return False
    }