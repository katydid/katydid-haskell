{-#LANGUAGE GADTs #-}

-- |
-- This module simplifies Relapse patterns.

module Simplify (
    simplify  
) where

import qualified Data.Set as S

import Ast
import Expr
import Exprs.Logic

-- |
-- simplify simplifies an input pattern to an equivalent simpler pattern.
simplify :: Grammar -> Pattern -> Pattern
simplify g pat =
    let simp = simplify' g
    in case pat of
    Empty -> Empty
    ZAny -> ZAny
    (Node v p) -> simplifyNode v (simp p)
    (Concat p1 p2) -> simplifyConcat (simp p1) (simp p2)
    (Or p1 p2) -> simplifyOr g (simp p1) (simp p2)
    (And p1 p2) -> simplifyAnd g (simp p1) (simp p2)
    (ZeroOrMore p) -> simplifyZeroOrMore (simp p)
    (Not p) -> simplifyNot (simp p)
    (Optional p) -> simplifyOptional (simp p)
    (Interleave p1 p2) -> simplifyInterleave (simp p1) (simp p2)
    (Contains p) -> simplifyContains (simp p)
    p@(Reference _) -> p

simplify' :: Grammar -> Pattern -> Pattern
simplify' g p = checkRef g $ simplify g p

simplifyNode :: Expr Bool -> Pattern -> Pattern
simplifyNode v p = case evalConst v of
    (Just False) -> Not ZAny
    _ -> Node v p

simplifyConcat :: Pattern -> Pattern -> Pattern
simplifyConcat (Not ZAny) _ = Not ZAny
simplifyConcat _ (Not ZAny) = Not ZAny
simplifyConcat (Concat p1 p2) p3 = 
    simplifyConcat p1 (Concat p2 p3)
simplifyConcat Empty p = p
simplifyConcat p Empty = p
simplifyConcat ZAny (Concat p ZAny) = Contains p
simplifyConcat p1 p2 = Concat p1 p2

simplifyOr :: Grammar -> Pattern -> Pattern -> Pattern
simplifyOr _ (Not ZAny) p = p
simplifyOr _ p (Not ZAny) = p
simplifyOr _ ZAny _ = ZAny
simplifyOr _ _ ZAny = ZAny
simplifyOr _ (Node v1 Empty) (Node v2 Empty) = Node (orExpr v1 v2) Empty
simplifyOr g Empty p 
    | nullable g p == Right True = p
    | otherwise = Or Empty p
simplifyOr g p Empty
    | nullable g p == Right True = p 
    | otherwise = Or Empty p
simplifyOr _ p1 p2 = bin Or $ simplifyChildren Or $ S.toAscList $ setOfOrs p1 `S.union` setOfOrs p2

simplifyChildren :: (Pattern -> Pattern -> Pattern) -> [Pattern] -> [Pattern]
simplifyChildren _ [] = []
simplifyChildren _ [p] = [p]
simplifyChildren op (p1@(Node v1 c1):(p2@(Node v2 c2):ps))
    | v1 == v2 = simplifyChildren op $ Node v1 (op c1 c2):ps
    | otherwise = p1:simplifyChildren op (p2:ps)
simplifyChildren op (p:ps) = p:simplifyChildren op ps

bin :: (Pattern -> Pattern -> Pattern) -> [Pattern] -> Pattern
bin op [p] = p
bin op [p1,p2] = op p1 p2
bin op (p:ps) = op p (bin op ps)

setOfOrs :: Pattern -> S.Set Pattern
setOfOrs (Or p1 p2) = setOfOrs p1 `S.union` setOfOrs p2
setOfOrs p = S.singleton p

simplifyAnd :: Grammar -> Pattern -> Pattern -> Pattern
simplifyAnd _ (Not ZAny) _ = Not ZAny
simplifyAnd _ _ (Not ZAny) = Not ZAny
simplifyAnd _ ZAny p = p
simplifyAnd _ p ZAny = p
simplifyAnd _ (Node v1 Empty) (Node v2 Empty) = Node (andExpr v1 v2) Empty
simplifyAnd g Empty p
    | nullable g p == Right True = Empty
    | otherwise = Not ZAny
simplifyAnd g p Empty
    | nullable g p == Right True = Empty
    | otherwise = Not ZAny
simplifyAnd _ p1 p2 = bin And $ simplifyChildren And $ S.toAscList $ setOfAnds p1 `S.union` setOfAnds p2

setOfAnds :: Pattern -> S.Set Pattern
setOfAnds (And p1 p2) = setOfAnds p1 `S.union` setOfAnds p2
setOfAnds p = S.singleton p

simplifyZeroOrMore :: Pattern -> Pattern
simplifyZeroOrMore (ZeroOrMore p) = ZeroOrMore p
simplifyZeroOrMore p = ZeroOrMore p

simplifyNot :: Pattern -> Pattern
simplifyNot (Not p) = p
simplifyNot p = Not p

simplifyOptional :: Pattern -> Pattern
simplifyOptional Empty = Empty
simplifyOptional p = Optional p

simplifyInterleave :: Pattern -> Pattern -> Pattern
simplifyInterleave (Not ZAny) _ = Not ZAny
simplifyInterleave _ (Not ZAny) = Not ZAny
simplifyInterleave Empty p = p
simplifyInterleave p Empty = p
simplifyInterleave ZAny ZAny = ZAny
simplifyInterleave p1 p2 = bin Interleave $ S.toAscList $ setOfInterleaves p1 `S.union` setOfInterleaves p2

setOfInterleaves :: Pattern -> S.Set Pattern
setOfInterleaves (Interleave p1 p2) = setOfInterleaves p1 `S.union` setOfInterleaves p2
setOfInterleaves p = S.singleton p

simplifyContains :: Pattern -> Pattern
simplifyContains Empty = ZAny
simplifyContains ZAny = ZAny
simplifyContains (Not ZAny) = Not ZAny
simplifyContains p = Contains p

checkRef :: Grammar -> Pattern -> Pattern
checkRef g p = case reverseLookupRef p g of
    Nothing     -> p
    (Just k)    -> Reference k

