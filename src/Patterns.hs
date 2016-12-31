module Patterns where

import qualified Data.Map.Strict as Map

type ValueType = String

data Value
	= Equal ValueType
	| OrValue Value Value
	| ExceptValue Value
	| AnyValue
	deriving (Eq, Ord, Show)

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