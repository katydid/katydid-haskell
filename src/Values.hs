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
simplifyValue (OrValue l r) = 
	let	sl = simplifyValue l
		sr = simplifyValue r
	in if sl == sr 
		then sl 
		else if sl == AnyValue || sr == AnyValue 
			then AnyValue
			else if sr == (NotValue AnyValue)
				then sl
				else if sl == (NotValue AnyValue)
					then sr
					else OrValue sl sr
simplifyValue (AndValue l r) =
	let	sl = simplifyValue l
		sr = simplifyValue r
	in if sl == sr 
		then sl
		else if sl == (NotValue AnyValue) || sr == (NotValue AnyValue)
			then (NotValue AnyValue)
			else if sr == AnyValue
				then sl
				else if sl == AnyValue
					then sr
					else AndValue sl sr
simplifyValue (NotValue v) =
	let sv = simplifyValue v
	in if sv == (NotValue v) then v else (NotValue v)
simplifyValue AnyValue = AnyValue

isFalse :: Value -> Bool
isFalse (Equal _) = False
isFalse (OrValue l r) = isFalse l && isFalse r
isFalse (AndValue l r) = isFalse l || isFalse r
isFalse (NotValue AnyValue) = True
isFalse (NotValue _) = False
isFalse AnyValue = False
