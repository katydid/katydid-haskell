module Values where

import Data.List (isInfixOf, isPrefixOf, isSuffixOf)

import Parsers

data BoolExpr
	= BoolConst Bool
	| BoolVariable

	| OrFunc BoolExpr BoolExpr
	| AndFunc BoolExpr BoolExpr
	| NotFunc BoolExpr

	| BoolEqualFunc BoolExpr BoolExpr
	| DoubleEqualFunc DoubleExpr DoubleExpr
	| IntEqualFunc IntExpr IntExpr
	| UintEqualFunc UintExpr UintExpr
	| StringEqualFunc StringExpr StringExpr
	| BytesEqualFunc BytesExpr BytesExpr

	| IntListContainsFunc IntExpr [IntExpr]
	| StringListContainsFunc StringExpr [StringExpr]
	| UintListContainsFunc UintExpr [UintExpr]
	| StringContainsFunc StringExpr StringExpr

	| BoolListElemFunc [BoolExpr] IntExpr

	| BytesGreaterOrEqualFunc BytesExpr BytesExpr
	| DoubleGreaterOrEqualFunc DoubleExpr DoubleExpr
	| IntGreaterOrEqualFunc IntExpr IntExpr
	| UintGreaterOrEqualFunc UintExpr UintExpr

	| BytesGreaterThanFunc BytesExpr BytesExpr
	| DoubleGreaterThanFunc DoubleExpr DoubleExpr
	| IntGreaterThanFunc IntExpr IntExpr
	| UintGreaterThanFunc UintExpr UintExpr

	| StringHasPrefixFunc StringExpr StringExpr
	| StringHasSuffixFunc StringExpr StringExpr

	| BytesLessOrEqualFunc BytesExpr BytesExpr
	| DoubleLessOrEqualFunc DoubleExpr DoubleExpr
	| IntLessOrEqualFunc IntExpr IntExpr
	| UintLessOrEqualFunc UintExpr UintExpr

	| BytesLessThanFunc BytesExpr BytesExpr
	| DoubleLessThanFunc DoubleExpr DoubleExpr
	| IntLessThanFunc IntExpr IntExpr
	| UintLessThanFunc UintExpr UintExpr

	| BytesNotEqualFunc BytesExpr BytesExpr
	| BoolNotEqualFunc BoolExpr BoolExpr
	| DoubleNotEqualFunc DoubleExpr DoubleExpr
	| IntNotEqualFunc IntExpr IntExpr
	| StringNotEqualFunc StringExpr StringExpr
	| UintNotEqualFunc UintExpr UintExpr

	| BytesTypeFunc BytesExpr
	| BoolTypeFunc BoolExpr
	| DoubleTypeFunc DoubleExpr
	| IntTypeFunc  IntExpr
	| UintTypeFunc UintExpr
	| StringTypeFunc StringExpr
	deriving (Eq, Ord, Show)

data DoubleExpr
	= DoubleConst Rational
	| DoubleVariable
	| DoubleListElemFunc [DoubleExpr] IntExpr
	deriving (Eq, Ord, Show)

data IntExpr
	= IntConst Int
	| IntVariable
	| IntListElemFunc [IntExpr] IntExpr

	| BytesListLengthFunc [BytesExpr]
	| BoolListLengthFunc [BoolExpr]
	| BytesLengthFunc BytesExpr
	| DoubleListLengthFunc [DoubleExpr]
	| IntListLengthFunc [IntExpr]
	| StringListLengthFunc [StringExpr]
	| UintListLengthFunc [UintExpr]
	| StringLengthFunc StringExpr
	deriving (Eq, Ord, Show)

data UintExpr
 	= UintConst Int
 	| UintVariable
 	| UintListElemFunc [UintExpr] IntExpr
 	deriving (Eq, Ord, Show)

data StringExpr
	= StringConst String
	| StringVariable
	| StringListElemFunc [StringExpr] IntExpr

	| StringToLowerFunc StringExpr
	| StringToUpperFunc StringExpr
	deriving (Eq, Ord, Show)

data BytesExpr
	= BytesConst String
	| BytesVariable
	| BytesListElemFunc [BytesExpr] IntExpr
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

evalBool (BoolEqualFunc e1 e2) v = do {
	v1 <- evalBool e1 v;
	v2 <- evalBool e2 v;
	return $ v1 == v2
}
evalBool (DoubleEqualFunc e1 e2) v = do {
	v1 <- evalDouble e1 v;
	v2 <- evalDouble e2 v;
	return $ v1 == v2
}
evalBool (IntEqualFunc e1 e2) v = do {
	v1 <- evalInt e1 v;
	v2 <- evalInt e2 v;
	return $ v1 == v2
}
evalBool (UintEqualFunc e1 e2) v = do {
	v1 <- evalUint e1 v;
	v2 <- evalUint e2 v;
	return $ v1 == v2
}
evalBool (StringEqualFunc e1 e2) v = do {
	v1 <- evalString e1 v;
	v2 <- evalString e2 v;
	return $ v1 == v2
}
evalBool (BytesEqualFunc e1 e2) v = do {
	v1 <- evalBytes e1 v;
	v2 <- evalBytes e2 v;
	return $ v1 == v2
}

evalBool (IntListContainsFunc e es) v = do {
	e' <- evalInt e v;
	es' <- mapM ((flip evalInt) v) es;
	return $ elem e es
}
evalBool (StringListContainsFunc e es) v = do {
	e' <- evalString e v;
	es' <- mapM ((flip evalString) v) es;
	return $ elem e es
}
evalBool (UintListContainsFunc e es) v = do {
	e' <- evalUint e v;
	es' <- mapM ((flip evalUint) v) es;
	return $ elem e es
}
evalBool (StringContainsFunc s sub) v = do {
	s' <- evalString s v;
	sub' <- evalString sub v;
	return $ isInfixOf sub' s'
}

evalBool (BoolListElemFunc es i) v = do {
	i' <- evalInt i v;
	es' <- mapM ((flip evalBool) v) es;
	return $ es' !! i'
}

evalBool (BytesGreaterOrEqualFunc e1 e2) v = do {
	v1 <- evalBytes e1 v;
	v2 <- evalBytes e2 v;
	return $ v1 >= v2
}
evalBool (DoubleGreaterOrEqualFunc e1 e2) v = do {
	v1 <- evalDouble e1 v;
	v2 <- evalDouble e2 v;
	return $ v1 >= v2
}
evalBool (IntGreaterOrEqualFunc e1 e2) v = do {
	v1 <- evalInt e1 v;
	v2 <- evalInt e2 v;
	return $ v1 >= v2
}
evalBool (UintGreaterOrEqualFunc e1 e2) v = do {
	v1 <- evalUint e1 v;
	v2 <- evalUint e2 v;
	return $ v1 >= v2
}

evalBool (BytesGreaterThanFunc e1 e2) v = do {
	v1 <- evalBytes e1 v;
	v2 <- evalBytes e2 v;
	return $ v1 > v2
}
evalBool (DoubleGreaterThanFunc e1 e2) v = do {
	v1 <- evalDouble e1 v;
	v2 <- evalDouble e2 v;
	return $ v1 > v2
}
evalBool (IntGreaterThanFunc e1 e2) v = do {
	v1 <- evalInt e1 v;
	v2 <- evalInt e2 v;
	return $ v1 > v2
}
evalBool (UintGreaterThanFunc e1 e2) v = do {
	v1 <- evalUint e1 v;
	v2 <- evalUint e2 v;
	return $ v1 > v2
}

evalBool (StringHasPrefixFunc e1 e2) v = do {
	v1 <- evalString e1 v;
	v2 <- evalString e2 v;
	return $ isPrefixOf v2 v1
}
evalBool (StringHasSuffixFunc e1 e2) v = do {
	v1 <- evalString e1 v;
	v2 <- evalString e2 v;
	return $ isSuffixOf v2 v1
}

evalBool (BytesLessOrEqualFunc e1 e2) v = do {
	v1 <- evalBytes e1 v;
	v2 <- evalBytes e2 v;
	return $ v1 <= v2
}
evalBool (DoubleLessOrEqualFunc e1 e2) v = do {
	v1 <- evalDouble e1 v;
	v2 <- evalDouble e2 v;
	return $ v1 <= v2
}
evalBool (IntLessOrEqualFunc e1 e2) v = do {
	v1 <- evalInt e1 v;
	v2 <- evalInt e2 v;
	return $ v1 <= v2
}
evalBool (UintLessOrEqualFunc e1 e2) v = do {
	v1 <- evalUint e1 v;
	v2 <- evalUint e2 v;
	return $ v1 <= v2
}

evalBool (BytesLessThanFunc e1 e2) v = do {
	v1 <- evalBytes e1 v;
	v2 <- evalBytes e2 v;
	return $ v1 < v2
}
evalBool (DoubleLessThanFunc e1 e2) v = do {
	v1 <- evalDouble e1 v;
	v2 <- evalDouble e2 v;
	return $ v1 < v2
}
evalBool (IntLessThanFunc e1 e2) v = do {
	v1 <- evalInt e1 v;
	v2 <- evalInt e2 v;
	return $ v1 < v2
}
evalBool (UintLessThanFunc e1 e2) v = do {
	v1 <- evalUint e1 v;
	v2 <- evalUint e2 v;
	return $ v1 < v2
}

evalDouble :: DoubleExpr -> Label -> Value Rational
evalDouble = error "todo"

evalInt :: IntExpr -> Label -> Value Int
evalInt = error "todo"

evalUint :: UintExpr -> Label -> Value Int
evalUint = error "todo"

evalString :: StringExpr -> Label -> Value String
evalString = error "todo"

evalBytes :: BytesExpr -> Label -> Value String
evalBytes = error "todo"

simplifyBoolExpr :: BoolExpr -> BoolExpr
simplifyBoolExpr e@(BoolEqualFunc (BoolConst b1) (BoolConst b2)) = BoolConst $ b1 == b2
simplifyBoolExpr (OrFunc v1 v2) = simplifyOrFunc (simplifyBoolExpr v1) (simplifyBoolExpr v2)
simplifyBoolExpr (AndFunc v1 v2) = simplifyAndFunc (simplifyBoolExpr v1) (simplifyBoolExpr v2)
simplifyBoolExpr (NotFunc v) = simplifyNotFunc (simplifyBoolExpr v)
simplifyBoolExpr v@(BoolConst _) = v

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