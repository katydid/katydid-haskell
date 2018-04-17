-- |
-- This module is a simple implementation of the internal derivative algorithm.
--
-- It is intended to be used for explanation purposes.
--
-- This means that it gives up speed for readability.
--
-- Thus it has no type of memoization.

module Derive (
    derive, calls, returns, zipderive
) where

import Data.Foldable (foldlM)

import Patterns
import Expr
import Parsers
import Simplify
import Zip
import IfExprs

-- | 
-- calls returns a compiled if expression tree.
-- Each if expression returns a child pattern, given the input value.
-- In other words calls signature is actually:
--
-- @
--   Refs -> [Pattern] -> Value -> [Pattern]
-- @
--
-- , where the resulting list of patterns are the child patterns,
-- that need to be derived given the trees child values.
calls :: Refs -> [Pattern] -> IfExprs
calls refs ps = compileIfExprs refs $ concatMap (\p -> deriveCall refs p []) ps

deriveCall :: Refs -> Pattern -> [IfExpr]-> [IfExpr]
deriveCall _ Empty res = res
deriveCall _ ZAny res = res
deriveCall _ (Node v p) res = newIfExpr v p (Not ZAny) : res
deriveCall refs (Concat l r) res
    | nullable refs l = deriveCall refs l (deriveCall refs r res)
    | otherwise = deriveCall refs l res
deriveCall refs (Or l r) res = deriveCall refs l (deriveCall refs r res)
deriveCall refs (And l r) res = deriveCall refs l (deriveCall refs r res)
deriveCall refs (Interleave l r) res = deriveCall refs l (deriveCall refs r res)
deriveCall refs (ZeroOrMore p) res = deriveCall refs p res
deriveCall refs (Reference name) res = deriveCall refs (lookupRef refs name) res
deriveCall refs (Not p) res = deriveCall refs p res
deriveCall refs (Contains p) res = deriveCall refs (Concat ZAny (Concat p ZAny)) res
deriveCall refs (Optional p) res = deriveCall refs (Or p Empty) res

-- |
-- returns takes a list of patterns and list of bools.
-- The list of bools represent the nullability of the derived child patterns.
-- Each bool will then replace each Node pattern with either an Empty or EmptySet.
-- The lists do not to be the same length, because each Pattern can contain an arbitrary number of Node Patterns.
returns :: Refs -> ([Pattern], [Bool]) -> [Pattern]
returns _ ([], []) = []
returns refs (p:tailps, ns) =
    let (dp, tailns) = deriveReturn refs p ns
        sp = simplify refs dp
    in  sp:returns refs (tailps, tailns)

deriveReturn :: Refs -> Pattern -> [Bool] -> (Pattern, [Bool])
deriveReturn _ Empty ns = (Not ZAny, ns)
deriveReturn _ ZAny ns = (ZAny, ns)
deriveReturn _ Node{} ns 
    | head ns = (Empty, tail ns)
    | otherwise = (Not ZAny, tail ns)
deriveReturn refs (Concat l r) ns
    | nullable refs l = 
            let (leftDeriv, leftTail) = deriveReturn refs l ns
                (rightDeriv, rightTail) = deriveReturn refs r leftTail
            in  (Or (Concat leftDeriv r) rightDeriv, rightTail)
    | otherwise = 
            let (leftDeriv, leftTail) = deriveReturn refs l ns
            in  (Concat leftDeriv r, leftTail)
deriveReturn refs (Or l r) ns = 
    let (leftDeriv, leftTail) = deriveReturn refs l ns
        (rightDeriv, rightTail) = deriveReturn refs r leftTail
    in (Or leftDeriv rightDeriv, rightTail)
deriveReturn refs (And l r) ns = 
    let (leftDeriv, leftTail) = deriveReturn refs l ns
        (rightDeriv, rightTail) = deriveReturn refs r leftTail
    in (And leftDeriv rightDeriv, rightTail)
deriveReturn refs (Interleave l r) ns = 
    let (leftDeriv, leftTail) = deriveReturn refs l ns
        (rightDeriv, rightTail) = deriveReturn refs r leftTail
    in (Or (Interleave leftDeriv r) (Interleave rightDeriv l), rightTail)
deriveReturn refs z@(ZeroOrMore p) ns = 
    let (derivp, tailns) = deriveReturn refs p ns
    in  (Concat derivp z, tailns)
deriveReturn refs (Reference name) ns = deriveReturn refs (lookupRef refs name) ns
deriveReturn refs (Not p) ns =
    let (derivp, tailns) = deriveReturn refs p ns
    in  (Not derivp, tailns)
deriveReturn refs (Contains p) ns = deriveReturn refs (Concat ZAny (Concat p ZAny)) ns
deriveReturn refs (Optional p) ns = deriveReturn refs (Or p Empty) ns

-- |
-- derive is the classic derivative implementation for trees.
derive :: Tree t => Refs -> [t] -> Either String Pattern
derive g ts = do {
    ps <- foldlM (deriv g) [lookupRef g "main"] ts;
    if length ps == 1 
        then return $ head ps
        else Left $ "Number of patterns is not one, but " ++ show ps
}

deriv :: Tree t => Refs -> [Pattern] -> t -> Either String [Pattern]
deriv refs ps tree =
    if all unescapable ps then return ps else
    let ifs = calls refs ps
        d = deriv refs
        nulls = map (nullable refs)
    in do {
        childps <- evalIfExprs ifs (getLabel tree);
        childres <- foldlM d childps (getChildren tree);
        return $ returns refs (ps, nulls childres);
    }

-- |
-- zipderive is a slighty optimized version of derivs.
-- It zips its intermediate pattern lists to reduce the state space.
zipderive :: Tree t => Refs -> [t] -> Either String Pattern
zipderive g ts = do {
    ps <- foldlM (zipderiv g) [lookupRef g "main"] ts;
    if length ps == 1 
        then return $ head ps
        else Left $ "Number of patterns is not one, but " ++ show ps
}

zipderiv :: Tree t => Refs -> [Pattern] -> t -> Either String [Pattern]
zipderiv refs ps tree =
    if all unescapable ps then return ps else
    let ifs = calls refs ps
        d = zipderiv refs
        nulls = map (nullable refs)
    in do {
        childps <- evalIfExprs ifs (getLabel tree);
        (zchildps, zipper) <- return $ zippy childps;
        childres <- foldlM d zchildps (getChildren tree);
        let unzipns = unzipby zipper (nulls childres)
        in return $ returns refs (ps, unzipns)
    }
