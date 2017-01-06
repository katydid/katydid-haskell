module Values where

import ParsedTree

data Value
	= Equal MyLabel
	| OrValue Value Value
	| AndValue Value Value
	| NotValue Value
	| AnyValue
	deriving (Eq, Ord, Show)

eval :: Value -> MyLabel -> Bool
eval (Equal value) v = v == value
eval (OrValue l r) v = eval l v || eval r v
eval (AndValue l r) v = eval l v && eval r v
eval (NotValue value) v = not $ eval value v
eval AnyValue _ = True

