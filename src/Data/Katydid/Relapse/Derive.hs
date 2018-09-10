-- |
-- This module is a simple implementation of the internal derivative algorithm.
--
-- It is intended to be used for explanation purposes.
--
-- This means that it gives up speed for readability.
--
-- Thus it has no type of memoization.

module Data.Katydid.Relapse.Derive (
    derive, calls, returns, zipderive
    -- * Internal functions
    -- | These functions are exposed for testing purposes.
    , removeOneForEach
) where

import Data.Foldable (foldlM)
import Data.List.Index (imap)

import Data.Katydid.Parser.Parser

import Data.Katydid.Relapse.Smart
import Data.Katydid.Relapse.Simplify
import Data.Katydid.Relapse.Zip
import Data.Katydid.Relapse.IfExprs

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
calls g ps = compileIfExprs $ concatMap (\p -> deriveCall g p []) ps

deriveCall :: Grammar -> Pattern -> [IfExpr] -> [IfExpr]
deriveCall _ Empty res = res
deriveCall _ ZAny res = res
deriveCall _ Node{expr=v,pat=p} res = newIfExpr v p emptySet : res
deriveCall g Concat{left=l,right=r} res
    | nullable l = deriveCall g l (deriveCall g r res)
    | otherwise = deriveCall g l res
deriveCall g Or{pats=ps} res = foldr (deriveCall g) res ps
deriveCall g And{pats=ps} res = foldr (deriveCall g) res ps
deriveCall g Interleave{pats=ps} res = foldr (deriveCall g) res ps
deriveCall g ZeroOrMore{pat=p} res = deriveCall g p res
deriveCall g Reference{refName=name} res = deriveCall g (lookupRef g name) res
deriveCall g Not{pat=p} res = deriveCall g p res
deriveCall g Contains{pat=p} res = deriveCall g p res
deriveCall g Optional{pat=p} res = deriveCall g p res

-- |
-- returns takes a list of patterns and list of bools.
-- The list of bools represent the nullability of the derived child patterns.
-- Each bool will then replace each Node pattern with either an Empty or EmptySet.
-- The lists do not to be the same length, because each Pattern can contain an arbitrary number of Node Patterns.
returns :: Grammar -> ([Pattern], [Bool]) -> [Pattern]
returns _ ([], []) = []
returns g (p:tailps, ns) =
    let (dp, tailns) = deriveReturn g p ns
    in  dp:returns g (tailps, tailns)

mapReturn :: Grammar -> [Pattern] -> [Bool] -> ([Pattern], [Bool])
mapReturn g ps ns = foldl (\(dps, tailns) p ->
        let (dp, tailoftail) = deriveReturn g p tailns
        in (dp:dps, tailoftail)
    ) ([], ns) ps

deriveReturn :: Grammar -> Pattern -> [Bool] -> (Pattern, [Bool])
deriveReturn _ Empty ns = (emptySet, ns)
deriveReturn _ ZAny ns = (zanyPat, ns)
deriveReturn _ Node{} ns
    | head ns = (emptyPat, tail ns)
    | otherwise = (emptySet, tail ns)
deriveReturn g Concat{left=l,right=r} ns
    | nullable l =
        let (dl, ltail) = deriveReturn g l ns
            (dr, rtail) = deriveReturn g r ltail
        in  (orPat (concatPat dl r) dr, rtail)
    | otherwise =
        let (dl, ltail) = deriveReturn g l ns
        in  (concatPat dl r, ltail)
deriveReturn g Or{pats=ps} ns =
    let (dps, tailns) = mapReturn g ps ns
    in (foldl1 orPat dps, tailns)
deriveReturn g And{pats=ps} ns =
    let (dps, tailns) = mapReturn g ps ns
    in (foldl1 andPat dps, tailns)
deriveReturn g Interleave{pats=ps} ns =
    let (dps, tailns) = mapReturn g ps ns
        pps = reverse $ removeOneForEach ps
        ips = zipWith (:) dps pps
        ors = map (foldl1 interleavePat) ips
    in (foldl1 orPat ors, tailns)
deriveReturn g z@ZeroOrMore{pat=p} ns =
    let (dp, tailns) = deriveReturn g p ns
    in  (concatPat dp z, tailns)
deriveReturn g Reference{refName=name} ns = deriveReturn g (lookupRef g name) ns
deriveReturn g Not{pat=p} ns =
    let (dp, tailns) = deriveReturn g p ns
    in  (notPat dp, tailns)
deriveReturn g c@Contains{pat=p} ns =
    let (dp, tailns) = deriveReturn g p ns
    in  (orPat c (containsPat dp), tailns)
deriveReturn g Optional{pat=p} ns = deriveReturn g p ns

-- | For internal testing.
-- removeOneForEach creates N copies of the list removing the n'th element from each.
removeOneForEach :: [a] -> [[a]]
removeOneForEach xs = imap (\index list ->
        let (start,end) = splitAt index list
        in start ++ tail end
    ) (replicate (length xs) xs)

-- |
-- derive is the classic derivative implementation for trees.
derive :: Tree t => Grammar -> [t] -> Either String Pattern
derive g ts = do {
    ps <- foldlM (deriv g) [lookupMain g] ts;
    if length ps == 1 
        then return $ head ps
        else Left $ "Number of patterns is not one, but " ++ show ps
}

deriv :: Tree t => Grammar -> [Pattern] -> t -> Either String [Pattern]
deriv g ps tree =
    if all unescapable ps then return ps else
    let ifs = calls g ps
        d = deriv g
        nulls = map nullable
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
    ps <- foldlM (zipderiv g) [lookupMain g] ts;
    if length ps == 1 
        then return $ head ps
        else Left $ "Number of patterns is not one, but " ++ show ps
}

zipderiv :: Tree t => Grammar -> [Pattern] -> t -> Either String [Pattern]
zipderiv g ps tree =
    if all unescapable ps then return ps else
    let ifs = calls g ps
        d = zipderiv g
        nulls = map nullable
    in do {
        childps <- evalIfExprs ifs (getLabel tree);
        (zchildps, zipper) <- return $ zippy childps;
        childres <- foldlM d zchildps (getChildren tree);
        let unzipns = unzipby zipper (nulls childres)
        in return $ returns g (ps, unzipns)
    }
