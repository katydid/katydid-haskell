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

