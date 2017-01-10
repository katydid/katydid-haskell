module Values where

import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Char (toLower, toUpper)

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
	deriving Show

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

eval :: BoolExpr -> Label -> Value Bool
eval = evalBool

evalBool :: BoolExpr -> Label -> Value Bool

evalBool (BoolConst b) _ = Value b
evalBool BoolVariable (Bool b) = Value b
evalBool BoolVariable l = Err $ "not a bool, but " ++ show l

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
evalBool (NotFunc e) v = case evalBool e v of
	(Value True) -> return False
	_ -> return True

evalBool (BoolEqualFunc e1 e2) v = eq (evalBool e1 v) (evalBool e2 v)
evalBool (DoubleEqualFunc e1 e2) v = eq (evalDouble e1 v) (evalDouble e2 v)
evalBool (IntEqualFunc e1 e2) v = eq (evalInt e1 v) (evalInt e2 v)
evalBool (UintEqualFunc e1 e2) v = eq (evalUint e1 v) (evalUint e2 v)
evalBool (StringEqualFunc e1 e2) v = eq (evalString e1 v) (evalString e2 v)
evalBool (BytesEqualFunc e1 e2) v = eq (evalBytes e1 v) (evalBytes e2 v)

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

evalBool (BoolNotEqualFunc e1 e2) v = do {
	v1 <- evalBool e1 v;
	v2 <- evalBool e2 v;
	return $ v1 /= v2
}
evalBool (DoubleNotEqualFunc e1 e2) v = do {
	v1 <- evalDouble e1 v;
	v2 <- evalDouble e2 v;
	return $ v1 /= v2
}
evalBool (IntNotEqualFunc e1 e2) v = do {
	v1 <- evalInt e1 v;
	v2 <- evalInt e2 v;
	return $ v1 /= v2
}
evalBool (UintNotEqualFunc e1 e2) v = do {
	v1 <- evalUint e1 v;
	v2 <- evalUint e2 v;
	return $ v1 /= v2
}
evalBool (StringNotEqualFunc e1 e2) v = do {
	v1 <- evalString e1 v;
	v2 <- evalString e2 v;
	return $ v1 /= v2
}
evalBool (BytesNotEqualFunc e1 e2) v = do {
	v1 <- evalBytes e1 v;
	v2 <- evalBytes e2 v;
	return $ v1 /= v2
}

evalBool (BytesTypeFunc e) v = case evalBytes e v of
	(Value _) -> Value True
	(Err _) -> Value False
evalBool (BoolTypeFunc e) v = case evalBool e v of
	(Value _) -> Value True
	(Err _) -> Value False
evalBool (DoubleTypeFunc e) v = case evalDouble e v of
	(Value _) -> Value True
	(Err _) -> Value False
evalBool (IntTypeFunc e) v = case evalInt e v of
	(Value _) -> Value True
	(Err _) -> Value False
evalBool (UintTypeFunc e) v = case evalUint e v of
	(Value _) -> Value True
	(Err _) -> Value False
evalBool (StringTypeFunc e) v = case evalString e v of
	(Value _) -> Value True
	(Err _) -> Value False

eq :: (Eq a) => (Value a) -> (Value a) -> Value Bool
eq (Value v1) (Value v2) = return $ v1 == v2
eq (Err _) _ = return False
eq _ (Err _) = return False

evalDouble :: DoubleExpr -> Label -> Value Rational
evalDouble (DoubleConst r) _ = Value r
evalDouble DoubleVariable (Number r) = Value r
evalDouble DoubleVariable l = Err $ "not a double, but " ++ show l

evalDouble (DoubleListElemFunc es i) v = do {
	i' <- evalInt i v;
	es' <- mapM ((flip evalDouble) v) es;
	return $ es' !! i'
}

evalInt :: IntExpr -> Label -> Value Int
evalInt (IntConst i) _ = Value i
evalInt IntVariable (Number r) = Value (truncate r)
evalInt IntVariable l = Err $ "not an int, but " ++ show l

evalInt (IntListElemFunc es i) v = do {
	i' <- evalInt i v;
	es' <- mapM ((flip evalInt) v) es;
	return $ es' !! i'
}

evalInt (BytesListLengthFunc es) v = do {
	es' <- mapM ((flip evalBytes) v) es;
	return $ length es'
}
evalInt (BoolListLengthFunc es) v = do {
	es' <- mapM ((flip evalBool) v) es;
	return $ length es'
}
evalInt (BytesLengthFunc e) v = do {
	e' <- evalBytes e v;
	return $ length e'
}
evalInt (DoubleListLengthFunc es) v = do {
	es' <- mapM ((flip evalDouble) v) es;
	return $ length es'
}
evalInt (IntListLengthFunc es) v = do {
	es' <- mapM ((flip evalInt) v) es;
	return $ length es'
}
evalInt (StringListLengthFunc es) v = do {
	es' <- mapM ((flip evalString) v) es;
	return $ length es'
}
evalInt (UintListLengthFunc es) v = do {
	es' <- mapM ((flip evalUint) v) es;
	return $ length es'
}
evalInt (StringLengthFunc e) v = do {
	e' <- evalString e v;
	return $ length e'
}

evalUint :: UintExpr -> Label -> Value Int
evalUint (UintConst i) _ = Value i
evalUint UintVariable (Number r) = Value (truncate r)
evalUint UintVariable l = Err $ "not a uint, but " ++ show l

evalUint (UintListElemFunc es i) v = do {
	i' <- evalInt i v;
	es' <- mapM ((flip evalUint) v) es;
	return $ es' !! i'
}

evalString :: StringExpr -> Label -> Value String
evalString (StringConst i) _ = Value i
evalString StringVariable (String s) = Value s
evalString StringVariable l = Err $ "not a string, but " ++ show l

evalString (StringListElemFunc es i) v = do {
	i' <- evalInt i v;
	es' <- mapM ((flip evalString) v) es;
	return $ es' !! i'
}

evalString (StringToLowerFunc s) v = do {
	s' <- evalString s v;
	return $ map toLower s'
}
evalString (StringToUpperFunc s) v = do {
	s' <- evalString s v;
	return $ map toUpper s'
}

evalBytes :: BytesExpr -> Label -> Value String
evalBytes (BytesConst u) _ = Value u
evalBytes BytesVariable (String s) = Value s
evalBytes BytesVariable l = Err $ "not bytes, but " ++ show l

evalBytes (BytesListElemFunc es i) v = do {
	i' <- evalInt i v;
	es' <- mapM ((flip evalBytes) v) es;
	return $ es' !! i'
}

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