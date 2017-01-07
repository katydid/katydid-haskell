module Values where

import Parsers

data Value
	= Equal Label
	| OrValue Value Value
	| AndValue Value Value
	| NotValue Value
	| AnyValue
	deriving (Eq, Ord, Show)

eval :: Value -> Label -> Bool
eval (Equal value) v = v == value
eval (OrValue l r) v = eval l v || eval r v
eval (AndValue l r) v = eval l v && eval r v
eval (NotValue value) v = not $ eval value v
eval AnyValue _ = True

