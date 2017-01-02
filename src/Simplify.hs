module Simplify where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Patterns
import Values

simplify :: Refs -> Pattern -> Pattern
simplify refs p =
	let simp = simplify' refs
	in case p of
	Empty -> Empty
	(Node v p) -> simplifyNode (simplifyValue v) (simp p)
 	(Concat p1 p2) -> simplifyConcat (simp p1) (simp p2)
 	(Or p1 p2) -> simplifyOr refs (simp p1) (simp p2)
 	(And p1 p2) -> simplifyAnd refs (simp p1) (simp p2)
 	(ZeroOrMore p) -> simplifyZeroOrMore (simp p)
 	(Not p) -> simplifyNot (simp p)
 	(Optional p) -> simplifyOptional (simp p)
 	(Interleave p1 p2) -> simplifyInterleave (simp p1) (simp p2)
 	(Contains p) -> simplifyContains (simp p)

simplify' :: Refs -> Pattern -> Pattern
simplify' refs p = checkRef refs $ simplify refs p

simplifyNode :: Value -> Pattern -> Pattern
simplifyNode (NotValue AnyValue) _ = Not ZAny
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
simplifyOr _ (Node v1 Empty) (Node v2 Empty) = Node (OrValue v1 v2) Empty
simplifyOr refs Empty p = if nullable refs p then p else Or Empty p
simplifyOr refs p Empty = if nullable refs p then p else Or Empty p
simplifyOr _ p1 p2 = bin Or $ simplifyChildren Or $ Set.toAscList $ setOfOrs p1 `Set.union` setOfOrs p2

simplifyChildren :: (Pattern -> Pattern -> Pattern) -> [Pattern] -> [Pattern]
simplifyChildren _ [] = []
simplifyChildren _ [p] = [p]
simplifyChildren op (p1@(Node v1 c1):(p2@(Node v2 c2):ps))
	| v1 == v2 = simplifyChildren op $ (Node v1 (op c1 c2)):ps
	| otherwise = p1:(simplifyChildren op (p2:ps))
simplifyChildren op (p:ps) = p:(simplifyChildren op ps)

bin :: (Pattern -> Pattern -> Pattern) -> [Pattern] -> Pattern
bin op (p1:p2:[]) = op p1 p2
bin op (p:ps) = op p (bin op ps)

setOfOrs :: Pattern -> Set.Set Pattern
setOfOrs (Or p1 p2) = setOfOrs p1 `Set.union` setOfOrs p2
setOfOrs p = Set.singleton p

simplifyAnd :: Refs -> Pattern -> Pattern -> Pattern
simplifyAnd _ (Not ZAny) _ = Not ZAny
simplifyAnd _ _ (Not ZAny) = Not ZAny
simplifyAnd _ ZAny p = p
simplifyAnd _ p ZAny = p
simplifyAnd _ (Node v1 Empty) (Node v2 Empty) = Node (AndValue v1 v2) Empty
simplifyAnd refs Empty p = if nullable refs p then Empty else Not ZAny
simplifyAnd refs p Empty = if nullable refs p then Empty else Not ZAny
simplifyAnd _ p1 p2 = bin And $ simplifyChildren And $ Set.toAscList $ setOfAnds p1 `Set.union` setOfAnds p2

setOfAnds :: Pattern -> Set.Set Pattern
setOfAnds (And p1 p2) = setOfAnds p1 `Set.union` setOfAnds p2
setOfAnds p = Set.singleton p

simplifyZeroOrMore :: Pattern -> Pattern
simplifyZeroOrMore (ZeroOrMore p) = (ZeroOrMore p)
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
simplifyInterleave p1 p2 = bin Interleave $ Set.toAscList $ setOfInterleaves p1 `Set.union` setOfInterleaves p2

setOfInterleaves :: Pattern -> Set.Set Pattern
setOfInterleaves (Interleave p1 p2) = setOfInterleaves p1 `Set.union` setOfInterleaves p2
setOfInterleaves p = Set.singleton p

simplifyContains :: Pattern -> Pattern
simplifyContains Empty = ZAny
simplifyContains ZAny = ZAny
simplifyContains (Not ZAny) = Not ZAny
simplifyContains p = Contains p

reverseLookup :: Pattern -> Refs -> Maybe String
reverseLookup p refs = case Map.keys $ Map.filter (== p) refs of
	[]  	-> Nothing
	(k:ks) 	-> Just k

checkRef :: Refs -> Pattern -> Pattern
checkRef refs p = case reverseLookup p refs of
	Nothing  	-> p
	(Just k) 	-> Reference k

simplifyValue :: Value -> Value
simplifyValue e@(Equal _) = e
simplifyValue (OrValue v1 v2) = simplifyOrValue (simplifyValue v1) (simplifyValue v2)
simplifyValue (AndValue v1 v2) = simplifyAndValue (simplifyValue v1) (simplifyValue v2)
simplifyValue (NotValue v) = simplifyNotValue (simplifyValue v)
simplifyValue AnyValue = AnyValue

simplifyOrValue :: Value -> Value -> Value
simplifyOrValue AnyValue _ = AnyValue
simplifyOrValue _ AnyValue = AnyValue
simplifyOrValue (NotValue AnyValue) v = v
simplifyOrValue v (NotValue AnyValue) = v
simplifyOrValue v1 v2
	| v1 == v2  = v1
	| v1 == (NotValue v2) = AnyValue
	| (NotValue v1) == v2 = AnyValue
	| otherwise = OrValue v1 v2

simplifyAndValue :: Value -> Value -> Value
simplifyAndValue AnyValue v = v
simplifyAndValue v AnyValue = v
simplifyAndValue (NotValue AnyValue) _ = NotValue AnyValue
simplifyAndValue _ (NotValue AnyValue) = NotValue AnyValue
simplifyAndValue v@(Equal v1) (Equal v2)
	| v1 == v2  = v
	| otherwise = (NotValue AnyValue)
simplifyAndValue v1 v2
	| v1 == v2  = v1
	| otherwise = AndValue v1 v2

simplifyNotValue :: Value -> Value
simplifyNotValue (NotValue v) = v
simplifyNotValue v = NotValue v
