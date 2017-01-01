module Patterns where

import qualified Data.Map.Strict as Map
import Values

data Pattern
	= Empty
	| ZAny
	| Node Value Pattern
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

type Refs = Map.Map String Pattern

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
nullable refs (Reference name) = nullable refs $ refs Map.! name

-- unescapable is used for short circuiting.
-- A part of the tree can be skipped if all patterns are unescapable.
unescapable :: Pattern -> Bool
unescapable ZAny = True
unescapable (Not ZAny) = True
unescapable _ = False