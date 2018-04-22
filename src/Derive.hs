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
calls :: Grammar -> [Pattern] -> IfExprs
calls g ps = compileIfExprs g $ concatMap (\p -> deriveCall g p []) ps

deriveCall :: Grammar -> Pattern -> [IfExpr]-> [IfExpr]
deriveCall _ Empty res = res
deriveCall _ ZAny res = res
deriveCall _ (Node v p) res = newIfExpr v p (Not ZAny) : res
deriveCall g (Concat l r) res
    | nullable g l = deriveCall g l (deriveCall g r res)
    | otherwise = deriveCall g l res
deriveCall g (Or l r) res = deriveCall g l (deriveCall g r res)
deriveCall g (And l r) res = deriveCall g l (deriveCall g r res)
deriveCall g (Interleave l r) res = deriveCall g l (deriveCall g r res)
deriveCall g (ZeroOrMore p) res = deriveCall g p res
deriveCall g (Reference name) res = deriveCall g (lookupRef g name) res
deriveCall g (Not p) res = deriveCall g p res
deriveCall g (Contains p) res = deriveCall g (Concat ZAny (Concat p ZAny)) res
deriveCall g (Optional p) res = deriveCall g (Or p Empty) res

-- |
-- returns takes a list of patterns and list of bools.
-- The list of bools represent the nullability of the derived child patterns.
-- Each bool will then replace each Node pattern with either an Empty or EmptySet.
-- The lists do not to be the same length, because each Pattern can contain an arbitrary number of Node Patterns.
returns :: Grammar -> ([Pattern], [Bool]) -> [Pattern]
returns _ ([], []) = []
returns g (p:tailps, ns) =
    let (dp, tailns) = deriveReturn g p ns
        sp = simplify g dp
    in  sp:returns g (tailps, tailns)

deriveReturn :: Grammar -> Pattern -> [Bool] -> (Pattern, [Bool])
deriveReturn _ Empty ns = (Not ZAny, ns)
deriveReturn _ ZAny ns = (ZAny, ns)
deriveReturn _ Node{} ns 
    | head ns = (Empty, tail ns)
    | otherwise = (Not ZAny, tail ns)
deriveReturn g (Concat l r) ns
    | nullable g l = 
            let (leftDeriv, leftTail) = deriveReturn g l ns
                (rightDeriv, rightTail) = deriveReturn g r leftTail
            in  (Or (Concat leftDeriv r) rightDeriv, rightTail)
    | otherwise = 
            let (leftDeriv, leftTail) = deriveReturn g l ns
            in  (Concat leftDeriv r, leftTail)
deriveReturn g (Or l r) ns = 
    let (leftDeriv, leftTail) = deriveReturn g l ns
        (rightDeriv, rightTail) = deriveReturn g r leftTail
    in (Or leftDeriv rightDeriv, rightTail)
deriveReturn g (And l r) ns = 
    let (leftDeriv, leftTail) = deriveReturn g l ns
        (rightDeriv, rightTail) = deriveReturn g r leftTail
    in (And leftDeriv rightDeriv, rightTail)
deriveReturn g (Interleave l r) ns = 
    let (leftDeriv, leftTail) = deriveReturn g l ns
        (rightDeriv, rightTail) = deriveReturn g r leftTail
    in (Or (Interleave leftDeriv r) (Interleave rightDeriv l), rightTail)
deriveReturn g z@(ZeroOrMore p) ns = 
    let (derivp, tailns) = deriveReturn g p ns
    in  (Concat derivp z, tailns)
deriveReturn g (Reference name) ns = deriveReturn g (lookupRef g name) ns
deriveReturn g (Not p) ns =
    let (derivp, tailns) = deriveReturn g p ns
    in  (Not derivp, tailns)
deriveReturn g (Contains p) ns = deriveReturn g (Concat ZAny (Concat p ZAny)) ns
deriveReturn g (Optional p) ns = deriveReturn g (Or p Empty) ns

-- |
-- derive is the classic derivative implementation for trees.
derive :: Tree t => Grammar -> [t] -> Either String Pattern
derive g ts = do {
    ps <- foldlM (deriv g) [lookupRef g "main"] ts;
    if length ps == 1 
        then return $ head ps
        else Left $ "Number of patterns is not one, but " ++ show ps
}

deriv :: Tree t => Grammar -> [Pattern] -> t -> Either String [Pattern]
deriv g ps tree =
    if all unescapable ps then return ps else
    let ifs = calls g ps
        d = deriv g
        nulls = map (nullable g)
    in do {
        childps <- evalIfExprs ifs (getLabel tree);
        childres <- foldlM d childps (getChildren tree);
        return $ returns g (ps, nulls childres);
    }

-- |
-- zipderive is a slighty optimized version of derivs.
-- It zips its intermediate pattern lists to reduce the state space.
zipderive :: Tree t => Grammar -> [t] -> Either String Pattern
zipderive g ts = do {
    ps <- foldlM (zipderiv g) [lookupRef g "main"] ts;
    if length ps == 1 
        then return $ head ps
        else Left $ "Number of patterns is not one, but " ++ show ps
}

zipderiv :: Tree t => Grammar -> [Pattern] -> t -> Either String [Pattern]
zipderiv g ps tree =
    if all unescapable ps then return ps else
    let ifs = calls g ps
        d = zipderiv g
        nulls = map (nullable g)
    in do {
        childps <- evalIfExprs ifs (getLabel tree);
        (zchildps, zipper) <- return $ zippy childps;
        childres <- foldlM d zchildps (getChildren tree);
        let unzipns = unzipby zipper (nulls childres)
        in return $ returns g (ps, unzipns)
    }
