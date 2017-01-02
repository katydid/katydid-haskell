module Values where

type ValueType = String

data Value
	= Equal ValueType
	| OrValue Value Value
	| AndValue Value Value
	| NotValue Value
	| AnyValue
	deriving (Eq, Ord, Show)

eval :: Value -> ValueType -> Bool
eval (Equal value) v = v == value
eval (OrValue l r) v = eval l v || eval r v
eval (AndValue l r) v = eval l v && eval r v
eval (NotValue value) v = not $ eval value v
eval AnyValue _ = True

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
