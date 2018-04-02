{-#LANGUAGE GADTs #-}

-- |
-- This module simplifies Relapse patterns.

module Simplify (
    simplify  
) where

import qualified Data.Set as S
import Control.Monad.Except (Except, runExcept, throwError)

import Patterns
import Expr
import Exprs.Logic



simplifyOr :: Refs -> Pattern -> Pattern -> Pattern
simplifyOr _ (Not ZAny) p = p
simplifyOr _ p (Not ZAny) = p
simplifyOr _ ZAny _ = ZAny
simplifyOr _ _ ZAny = ZAny
simplifyOr _ (Node v1 Empty) (Node v2 Empty) = Node (orExpr v1 v2) Empty
simplifyOr refs Empty p 
    | nullable refs p = p
    | otherwise = Or Empty p
simplifyOr refs p Empty
    | nullable refs p = p 
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

simplifyAnd :: Refs -> Pattern -> Pattern -> Pattern
simplifyAnd _ (Not ZAny) _ = Not ZAny
simplifyAnd _ _ (Not ZAny) = Not ZAny
simplifyAnd _ ZAny p = p
simplifyAnd _ p ZAny = p
simplifyAnd _ (Node v1 Empty) (Node v2 Empty) = Node (andExpr v1 v2) Empty
simplifyAnd refs Empty p
    | nullable refs p = Empty
    | otherwise = Not ZAny
simplifyAnd refs p Empty
    | nullable refs p = Empty
    | otherwise = Not ZAny
simplifyAnd _ p1 p2 = bin And $ simplifyChildren And $ S.toAscList $ setOfAnds p1 `S.union` setOfAnds p2

setOfAnds :: Pattern -> S.Set Pattern
setOfAnds (And p1 p2) = setOfAnds p1 `S.union` setOfAnds p2
setOfAnds p = S.singleton p



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




