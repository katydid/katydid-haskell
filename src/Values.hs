module Values where

import Parsers

data BoolExpr
	= BoolConst Bool
	| BoolVariable
	| BoolEqualFunc BoolExpr BoolExpr
	| DoubleEqualFunc DoubleExpr DoubleExpr
	| IntEqualFunc IntExpr IntExpr
	| UintEqualFunc UintExpr UintExpr
	| StringEqualFunc StringExpr StringExpr
	| BytesEqualFunc BytesExpr BytesExpr
	| OrFunc BoolExpr BoolExpr
	| AndFunc BoolExpr BoolExpr
	| NotFunc BoolExpr
	deriving (Eq, Ord, Show)

data DoubleExpr
	= DoubleConst Rational
	| DoubleVariable
	deriving (Eq, Ord, Show)

data IntExpr
	= IntConst Int
	| IntVariable
	deriving (Eq, Ord, Show)

data UintExpr
 	= UintConst Int
 	| UintVariable
 	deriving (Eq, Ord, Show)

data StringExpr
	= StringConst String
	| StringVariable
	deriving (Eq, Ord, Show)

data BytesExpr
	= BytesConst String
	| BytesVariable
	deriving (Eq, Ord, Show)


data Value a = Err String
	| Value a

-- instance Functor Value where
--   fmap = liftM
instance Functor Value where
	fmap f (Value v) = Value (f v)
	fmap f (Err s) = Err s

-- instance Applicative Value where
--   pure  = return
--   (<*>) = ap
instance Applicative Value where
	pure = Value
	(Value f) <*> (Value v) = Value $ f v
	(Value _) <*> (Err s) = (Err s)
	(Err s) <*> (Value _) = (Err s)
	(Err s1) <*> (Err s2) = (Err $ s1 ++ s2)

instance Monad Value where
    (Value v) >>= f = f v
    (Err s) >>= _ = Err s
    fail e = Err e
    return v = Value v

eval :: BoolExpr -> Label -> Bool
eval e l = case evalBool e l of
	(Value v) -> v
	(Err errStr) -> error errStr

evalBool :: BoolExpr -> Label -> Value Bool
evalBool (BoolConst b) _ = Value b
evalBool BoolVariable (Bool b) = Value b
evalBool BoolVariable _ = Err "not a bool"
evalBool (BoolEqualFunc e1 e2) v = do {
	b1 <- evalBool e1 v;
	b2 <- evalBool e2 v;
	return $ b1 == b2
}
evalBool (OrFunc e1 e2) v = do {
	b1 <- evalBool e1 v;
	b2 <- evalBool e2 v;
	return $ b1 || b2
}
evalBool (AndFunc e1 e2) v = do {
	b1 <- evalBool e1 v;
	b2 <- evalBool e2 v;
	return $ b1 && b2
}
evalBool (NotFunc e) v = do {
	b <- evalBool e v;
	return $ not b
}


simplifyValue :: BoolExpr -> BoolExpr
simplifyValue e@(BoolEqualFunc (BoolConst b1) (BoolConst b2)) = BoolConst $ b1 == b2
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
simplifyAndFunc v1 v2
	| v1 == v2  = v1
	| otherwise = AndFunc v1 v2

simplifyNotFunc :: BoolExpr -> BoolExpr
simplifyNotFunc (NotFunc v) = v
simplifyNotFunc (BoolConst True) = (BoolConst False)
simplifyNotFunc (BoolConst False) = (BoolConst True)
simplifyNotFunc v = NotFunc v