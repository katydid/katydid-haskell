module Values where

import Parsers

data BoolExpr
	= BoolConst Bool
	| EqualFunc Label
	| OrFunc BoolExpr BoolExpr
	| AndFunc BoolExpr BoolExpr
	| NotFunc BoolExpr
	deriving (Eq, Ord, Show)

eval :: BoolExpr -> Label -> Bool
eval (EqualFunc value) v = v == value
eval (OrFunc l r) v = eval l v || eval r v
eval (AndFunc l r) v = eval l v && eval r v
eval (NotFunc value) v = not $ eval value v
eval (BoolConst b) _ = b

simplifyValue :: BoolExpr -> BoolExpr
simplifyValue e@(EqualFunc _) = e
simplifyValue (OrFunc v1 v2) = simplifyOrFunc (simplifyValue v1) (simplifyValue v2)
simplifyValue (AndFunc v1 v2) = simplifyAndFunc (simplifyValue v1) (simplifyValue v2)
simplifyValue (NotFunc v) = simplifyNotFunc (simplifyValue v)
simplifyValue v@(BoolConst _) = v

simplifyOrFunc :: BoolExpr -> BoolExpr -> BoolExpr
simplifyOrFunc true@(BoolConst True) _ = true
simplifyOrFunc _ true@(BoolConst True) = true
simplifyOrFunc (BoolConst False) v = v
simplifyOrFunc v (BoolConst False) = v
simplifyOrFunc v1 v2
	| v1 == v2  = v1
	| otherwise = OrFunc v1 v2

simplifyAndFunc :: BoolExpr -> BoolExpr -> BoolExpr
simplifyAndFunc (BoolConst True) v = v
simplifyAndFunc v (BoolConst True) = v
simplifyAndFunc false@(BoolConst False) _ = false
simplifyAndFunc _ false@(BoolConst False) = false
simplifyAndFunc v@(EqualFunc v1) (EqualFunc v2)
	| v1 == v2  = v
	| otherwise = (BoolConst False)
simplifyAndFunc v1 v2
	| v1 == v2  = v1
	| otherwise = AndFunc v1 v2

simplifyNotFunc :: BoolExpr -> BoolExpr
simplifyNotFunc (NotFunc v) = v
simplifyNotFunc (BoolConst True) = (BoolConst False)
simplifyNotFunc (BoolConst False) = (BoolConst True)
simplifyNotFunc v = NotFunc v