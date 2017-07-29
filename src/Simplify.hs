module Simplify (
    simplify  
) where

import qualified Data.Set as DataSet

import Patterns
import Values

simplify :: Refs -> Pattern -> Pattern
simplify refs pattern =
    let simp = simplify' refs
    in case pattern of
    Empty -> Empty
    ZAny -> ZAny
    (Node v p) -> simplifyNode (simplifyBoolExpr v) (simp p)
    (Concat p1 p2) -> simplifyConcat (simp p1) (simp p2)
    (Or p1 p2) -> simplifyOr refs (simp p1) (simp p2)
    (And p1 p2) -> simplifyAnd refs (simp p1) (simp p2)
    (ZeroOrMore p) -> simplifyZeroOrMore (simp p)
    (Not p) -> simplifyNot (simp p)
    (Optional p) -> simplifyOptional (simp p)
    (Interleave p1 p2) -> simplifyInterleave (simp p1) (simp p2)
    (Contains p) -> simplifyContains (simp p)
    p@(Reference _) -> p

simplify' :: Refs -> Pattern -> Pattern
simplify' refs p = checkRef refs $ simplify refs p

simplifyNode :: BoolExpr -> Pattern -> Pattern
simplifyNode (BoolConst False) _ = Not ZAny
simplifyNode v p = Node v p

simplifyConcat :: Pattern -> Pattern -> Pattern
simplifyConcat (Not ZAny) _ = Not ZAny
simplifyConcat _ (Not ZAny) = Not ZAny
simplifyConcat (Concat p1 p2) p3 = 
    simplifyConcat p1 (Concat p2 p3)
simplifyConcat Empty p = p
simplifyConcat p Empty = p
simplifyConcat ZAny (Concat p ZAny) = Contains p
simplifyConcat p1 p2 = Concat p1 p2

simplifyOr :: Refs -> Pattern -> Pattern -> Pattern
simplifyOr _ (Not ZAny) p = p
simplifyOr _ p (Not ZAny) = p
simplifyOr _ ZAny _ = ZAny
simplifyOr _ _ ZAny = ZAny
simplifyOr _ (Node v1 Empty) (Node v2 Empty) = Node (OrFunc v1 v2) Empty
simplifyOr refs Empty p = if nullable refs p then p else Or Empty p
simplifyOr refs p Empty = if nullable refs p then p else Or Empty p
simplifyOr _ p1 p2 = bin Or $ simplifyChildren Or $ DataSet.toAscList $ setOfOrs p1 `DataSet.union` setOfOrs p2

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

setOfOrs :: Pattern -> DataSet.Set Pattern
setOfOrs (Or p1 p2) = setOfOrs p1 `DataSet.union` setOfOrs p2
setOfOrs p = DataSet.singleton p

simplifyAnd :: Refs -> Pattern -> Pattern -> Pattern
simplifyAnd _ (Not ZAny) _ = Not ZAny
simplifyAnd _ _ (Not ZAny) = Not ZAny
simplifyAnd _ ZAny p = p
simplifyAnd _ p ZAny = p
simplifyAnd _ (Node v1 Empty) (Node v2 Empty) = Node (AndFunc v1 v2) Empty
simplifyAnd refs Empty p = if nullable refs p then Empty else Not ZAny
simplifyAnd refs p Empty = if nullable refs p then Empty else Not ZAny
simplifyAnd _ p1 p2 = bin And $ simplifyChildren And $ DataSet.toAscList $ setOfAnds p1 `DataSet.union` setOfAnds p2

setOfAnds :: Pattern -> DataSet.Set Pattern
setOfAnds (And p1 p2) = setOfAnds p1 `DataSet.union` setOfAnds p2
setOfAnds p = DataSet.singleton p

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
simplifyInterleave p1 p2 = bin Interleave $ DataSet.toAscList $ setOfInterleaves p1 `DataSet.union` setOfInterleaves p2

setOfInterleaves :: Pattern -> DataSet.Set Pattern
setOfInterleaves (Interleave p1 p2) = setOfInterleaves p1 `DataSet.union` setOfInterleaves p2
setOfInterleaves p = DataSet.singleton p

simplifyContains :: Pattern -> Pattern
simplifyContains Empty = ZAny
simplifyContains ZAny = ZAny
simplifyContains (Not ZAny) = Not ZAny
simplifyContains p = Contains p

checkRef :: Refs -> Pattern -> Pattern
checkRef refs p = case reverseLookupRef p refs of
    Nothing  	-> p
    (Just k) 	-> Reference k

