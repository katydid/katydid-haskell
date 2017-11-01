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
import Control.Monad.Except (Except, mapExcept, throwError)

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
calls refs ps = compileIfExprs refs $ concatMap (deriveCall refs) ps

deriveCall :: Refs -> Pattern -> [IfExpr]
deriveCall _ Empty = []
deriveCall _ ZAny = []
deriveCall _ (Node v p) = [newIfExpr v p (Not ZAny)]
deriveCall refs (Concat l r)
    | nullable refs l = deriveCall refs l ++ deriveCall refs r
    | otherwise = deriveCall refs l
deriveCall refs (Or l r) = deriveCall refs l ++ deriveCall refs r
deriveCall refs (And l r) = deriveCall refs l ++ deriveCall refs r
deriveCall refs (Interleave l r) = deriveCall refs l ++ deriveCall refs r
deriveCall refs (ZeroOrMore p) = deriveCall refs p
deriveCall refs (Reference name) = deriveCall refs $ lookupRef refs name
deriveCall refs (Not p) = deriveCall refs p
deriveCall refs (Contains p) = deriveCall refs (Concat ZAny (Concat p ZAny))
deriveCall refs (Optional p) = deriveCall refs (Or p Empty)

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

onePattern :: Either ValueErr [Pattern] -> Either String Pattern
onePattern (Right [r]) = return r
onePattern (Left e) = throwError $ show e
onePattern (Right rs) = throwError $ "Number of patterns is not one, but " ++ show rs

-- |
-- derive is the classic derivative implementation for trees.
derive :: Tree t => Refs -> [t] -> Except String Pattern
derive g ts = mapExcept onePattern $ foldlM (deriv g) [lookupRef g "main"] ts

deriv :: Tree t => Refs -> [Pattern] -> t -> Except ValueErr [Pattern]
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
zipderive :: Tree t => Refs -> [t] -> Except String Pattern
zipderive g ts = mapExcept onePattern $ foldlM (zipderiv g) [lookupRef g "main"] ts

zipderiv :: Tree t => Refs -> [Pattern] -> t -> Except ValueErr [Pattern]
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
