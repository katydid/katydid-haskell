module Values where

import Parsers

data BoolExpr
	= BoolConst Bool
	| EqualFunc Label
	| OrValue BoolExpr BoolExpr
	| AndValue BoolExpr BoolExpr
	| NotValue BoolExpr
	deriving (Eq, Ord, Show)

eval :: BoolExpr -> Label -> Bool
eval (EqualFunc value) v = v == value
eval (OrValue l r) v = eval l v || eval r v
eval (AndValue l r) v = eval l v && eval r v
eval (NotValue value) v = not $ eval value v
eval (BoolConst b) _ = b

simplifyValue :: BoolExpr -> BoolExpr
simplifyValue e@(EqualFunc _) = e
simplifyValue (OrValue v1 v2) = simplifyOrValue (simplifyValue v1) (simplifyValue v2)
simplifyValue (AndValue v1 v2) = simplifyAndValue (simplifyValue v1) (simplifyValue v2)
simplifyValue (NotValue v) = simplifyNotValue (simplifyValue v)
simplifyValue v@(BoolConst _) = v

simplifyOrValue :: BoolExpr -> BoolExpr -> BoolExpr
simplifyOrValue true@(BoolConst True) _ = true
simplifyOrValue _ true@(BoolConst True) = true
simplifyOrValue (BoolConst False) v = v
simplifyOrValue v (BoolConst False) = v
simplifyOrValue v1 v2
	| v1 == v2  = v1
	| otherwise = OrValue v1 v2

simplifyAndValue :: BoolExpr -> BoolExpr -> BoolExpr
simplifyAndValue (BoolConst True) v = v
simplifyAndValue v (BoolConst True) = v
simplifyAndValue false@(BoolConst False) _ = false
simplifyAndValue _ false@(BoolConst False) = false
simplifyAndValue v@(EqualFunc v1) (EqualFunc v2)
	| v1 == v2  = v
	| otherwise = (BoolConst False)
simplifyAndValue v1 v2
	| v1 == v2  = v1
	| otherwise = AndValue v1 v2

simplifyNotValue :: BoolExpr -> BoolExpr
simplifyNotValue (NotValue v) = v
simplifyNotValue (BoolConst True) = (BoolConst False)
simplifyNotValue (BoolConst False) = (BoolConst True)
simplifyNotValue v = NotValue v