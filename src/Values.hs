module Values where

import Parsers

data BoolExpr
	= EqualFunc Label
	| OrValue BoolExpr BoolExpr
	| AndValue BoolExpr BoolExpr
	| NotValue BoolExpr
	| AnyValue
	deriving (Eq, Ord, Show)

eval :: BoolExpr -> Label -> Bool
eval (EqualFunc value) v = v == value
eval (OrValue l r) v = eval l v || eval r v
eval (AndValue l r) v = eval l v && eval r v
eval (NotValue value) v = not $ eval value v
eval AnyValue _ = True

simplifyValue :: BoolExpr -> BoolExpr
simplifyValue e@(EqualFunc _) = e
simplifyValue (OrValue v1 v2) = simplifyOrValue (simplifyValue v1) (simplifyValue v2)
simplifyValue (AndValue v1 v2) = simplifyAndValue (simplifyValue v1) (simplifyValue v2)
simplifyValue (NotValue v) = simplifyNotValue (simplifyValue v)
simplifyValue AnyValue = AnyValue

simplifyOrValue :: BoolExpr -> BoolExpr -> BoolExpr
simplifyOrValue AnyValue _ = AnyValue
simplifyOrValue _ AnyValue = AnyValue
simplifyOrValue (NotValue AnyValue) v = v
simplifyOrValue v (NotValue AnyValue) = v
simplifyOrValue v1 v2
	| v1 == v2  = v1
	| v1 == (NotValue v2) = AnyValue
	| (NotValue v1) == v2 = AnyValue
	| otherwise = OrValue v1 v2

simplifyAndValue :: BoolExpr -> BoolExpr -> BoolExpr
simplifyAndValue AnyValue v = v
simplifyAndValue v AnyValue = v
simplifyAndValue (NotValue AnyValue) _ = NotValue AnyValue
simplifyAndValue _ (NotValue AnyValue) = NotValue AnyValue
simplifyAndValue v@(EqualFunc v1) (EqualFunc v2)
	| v1 == v2  = v
	| otherwise = (NotValue AnyValue)
simplifyAndValue v1 v2
	| v1 == v2  = v1
	| otherwise = AndValue v1 v2

simplifyNotValue :: BoolExpr -> BoolExpr
simplifyNotValue (NotValue v) = v
simplifyNotValue v = NotValue v