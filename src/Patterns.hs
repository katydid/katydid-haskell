module Patterns where

import qualified Data.Map.Strict as DataMap
import qualified Data.Set as DataSet

import Values

data Pattern
	= Empty
	| ZAny
	| Node BoolExpr Pattern
	| Or Pattern Pattern
	| And Pattern Pattern
	| Not Pattern
	| Concat Pattern Pattern
	| Interleave Pattern Pattern
	| ZeroOrMore Pattern
	| Optional Pattern
	| Contains Pattern
	| Reference String
	deriving (Eq, Ord, Show)

nullable :: Refs -> Pattern -> Bool
nullable refs Empty = True
nullable refs ZAny = True
nullable refs (Node _ _) = False
nullable refs (Or l r) = nullable refs l || nullable refs r
nullable refs (And l r) = nullable refs l && nullable refs r
nullable refs (Not p) = not $ nullable refs p
nullable refs (Concat l r) = nullable refs l && nullable refs r
nullable refs (Interleave l r) = nullable refs l && nullable refs r
nullable refs (ZeroOrMore _) = True
nullable refs (Optional _) = True
nullable refs (Contains p) = nullable refs p
nullable refs (Reference name) = nullable refs $ lookupRef refs name

-- unescapable is used for short circuiting.
-- A part of the tree can be skipped if all patterns are unescapable.
unescapable :: Pattern -> Bool
unescapable ZAny = True
unescapable (Not ZAny) = True
unescapable _ = False

newtype Refs = Refs (DataMap.Map String Pattern)
	deriving (Show, Eq)

lookupRef :: Refs -> String -> Pattern
lookupRef (Refs m) name = m DataMap.! name

reverseLookupRef :: Pattern -> Refs -> Maybe String
reverseLookupRef p (Refs m) = case DataMap.keys $ DataMap.filter (== p) m of
	[]  	-> Nothing
	(k:ks) 	-> Just k

newRef :: String -> Pattern -> Refs
newRef key value = Refs $ DataMap.singleton key value

emptyRef :: Refs
emptyRef = Refs DataMap.empty

union :: Refs -> Refs -> Refs
union (Refs m1) (Refs m2) = Refs $ DataMap.union m1 m2 

hasRecursion :: Refs -> Bool
hasRecursion refs = hasRec refs (DataSet.singleton "main") (lookupRef refs "main")

hasRec :: Refs -> DataSet.Set String -> Pattern -> Bool
hasRec refs set Empty = False
hasRec refs set ZAny = False
hasRec refs set (Node _ _) = False
hasRec refs set (Or l r) = hasRec refs set l || hasRec refs set r
hasRec refs set (And l r) = hasRec refs set l || hasRec refs set r
hasRec refs set (Not p) = hasRec refs set p
hasRec refs set (Concat l r) = hasRec refs set l || (nullable refs l && hasRec refs set r)
hasRec refs set (Interleave l r) = hasRec refs set l || hasRec refs set r
hasRec refs set (ZeroOrMore _) = False
hasRec refs set (Optional p) = hasRec refs set p
hasRec refs set (Contains p) = hasRec refs set p
hasRec refs set (Reference name) = DataSet.member name set || hasRec refs (DataSet.insert name set) (lookupRef refs name)
