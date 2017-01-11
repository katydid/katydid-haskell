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

evalBool (DoubleGreaterOrEqualFunc e1 e2) v = ge (evalDouble e1 v) (evalDouble e2 v)
evalBool (IntGreaterOrEqualFunc e1 e2) v = ge (evalInt e1 v) (evalInt e2 v)
evalBool (UintGreaterOrEqualFunc e1 e2) v = ge (evalUint e1 v) (evalUint e2 v)
evalBool (BytesGreaterOrEqualFunc e1 e2) v = ge (evalBytes e1 v) (evalBytes e2 v)

evalBool (DoubleGreaterThanFunc e1 e2) v = gt (evalDouble e1 v) (evalDouble e2 v)
evalBool (IntGreaterThanFunc e1 e2) v = gt (evalInt e1 v) (evalInt e2 v)
evalBool (UintGreaterThanFunc e1 e2) v = gt (evalUint e1 v) (evalUint e2 v)
evalBool (BytesGreaterThanFunc e1 e2) v = gt (evalBytes e1 v) (evalBytes e2 v)

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

evalBool (DoubleLessOrEqualFunc e1 e2) v = le (evalDouble e1 v) (evalDouble e2 v)
evalBool (IntLessOrEqualFunc e1 e2) v = le (evalInt e1 v) (evalInt e2 v)
evalBool (UintLessOrEqualFunc e1 e2) v = le (evalUint e1 v) (evalUint e2 v)
evalBool (BytesLessOrEqualFunc e1 e2) v = le (evalBytes e1 v) (evalBytes e2 v)

evalBool (DoubleLessThanFunc e1 e2) v = lt (evalDouble e1 v) (evalDouble e2 v)
evalBool (IntLessThanFunc e1 e2) v = lt (evalInt e1 v) (evalInt e2 v)
evalBool (UintLessThanFunc e1 e2) v = lt (evalUint e1 v) (evalUint e2 v)
evalBool (BytesLessThanFunc e1 e2) v = lt (evalBytes e1 v) (evalBytes e2 v)

evalBool (BoolNotEqualFunc e1 e2) v = ne (evalBool e1 v) (evalBool e2 v)
evalBool (DoubleNotEqualFunc e1 e2) v = ne (evalDouble e1 v) (evalDouble e2 v)
evalBool (IntNotEqualFunc e1 e2) v = ne (evalInt e1 v) (evalInt e2 v)
evalBool (UintNotEqualFunc e1 e2) v = ne (evalUint e1 v) (evalUint e2 v)
evalBool (StringNotEqualFunc e1 e2) v = ne (evalString e1 v) (evalString e2 v)
evalBool (BytesNotEqualFunc e1 e2) v = ne (evalBytes e1 v) (evalBytes e2 v)

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

ge :: (Ord a) => (Value a) -> (Value a) -> Value Bool
ge (Value v1) (Value v2) = return $ v1 >= v2
ge (Err _) _ = return False
ge _ (Err _) = return False

gt :: (Ord a) => (Value a) -> (Value a) -> Value Bool
gt (Value v1) (Value v2) = return $ v1 > v2
gt (Err _) _ = return False
gt _ (Err _) = return False

le :: (Ord a) => (Value a) -> (Value a) -> Value Bool
le (Value v1) (Value v2) = return $ v1 <= v2
le (Err _) _ = return False
le _ (Err _) = return False

lt :: (Ord a) => (Value a) -> (Value a) -> Value Bool
lt (Value v1) (Value v2) = return $ v1 < v2
lt (Err _) _ = return False
lt _ (Err _) = return False

ne :: (Eq a) => (Value a) -> (Value a) -> Value Bool
ne (Value v1) (Value v2) = return $ v1 /= v2
ne (Err _) _ = return False
ne _ (Err _) = return False

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
simplifyBoolExpr v@(BoolConst _) = v
simplifyBoolExpr v@BoolVariable = v

simplifyBoolExpr (OrFunc v1 v2) = simplifyOrFunc (simplifyBoolExpr v1) (simplifyBoolExpr v2)
simplifyBoolExpr (AndFunc v1 v2) = simplifyAndFunc (simplifyBoolExpr v1) (simplifyBoolExpr v2)
simplifyBoolExpr (NotFunc v) = simplifyNotFunc (simplifyBoolExpr v)

simplifyBoolExpr (BoolEqualFunc e1 e2) = BoolEqualFunc (simplifyBoolExpr e1) (simplifyBoolExpr e2)
simplifyBoolExpr (DoubleEqualFunc e1 e2) = DoubleEqualFunc (simplifyDoubleExpr e1) (simplifyDoubleExpr e2)
simplifyBoolExpr (IntEqualFunc e1 e2) = IntEqualFunc (simplifyIntExpr e1) (simplifyIntExpr e2)
simplifyBoolExpr (UintEqualFunc e1 e2) = UintEqualFunc (simplifyUintExpr e1) (simplifyUintExpr e2)
simplifyBoolExpr (StringEqualFunc e1 e2) = StringEqualFunc (simplifyStringExpr e1) (simplifyStringExpr e2)
simplifyBoolExpr (BytesEqualFunc e1 e2) = BytesEqualFunc (simplifyBytesExpr e1) (simplifyBytesExpr e2)

simplifyBoolExpr (IntListContainsFunc e es) = IntListContainsFunc (simplifyIntExpr e) (map simplifyIntExpr es)
simplifyBoolExpr (StringListContainsFunc e es) = StringListContainsFunc (simplifyStringExpr e) (map simplifyStringExpr es)
simplifyBoolExpr (UintListContainsFunc e es) = UintListContainsFunc (simplifyUintExpr e) (map simplifyUintExpr es)
simplifyBoolExpr (StringContainsFunc e1 e2) = StringContainsFunc (simplifyStringExpr e1) (simplifyStringExpr e2)

simplifyBoolExpr (BoolListElemFunc es e) = BoolListElemFunc (map simplifyBoolExpr es) (simplifyIntExpr e)

simplifyBoolExpr (BytesGreaterOrEqualFunc e1 e2) = BytesGreaterOrEqualFunc (simplifyBytesExpr e1) (simplifyBytesExpr e2)
simplifyBoolExpr (DoubleGreaterOrEqualFunc e1 e2) = DoubleGreaterOrEqualFunc (simplifyDoubleExpr e1) (simplifyDoubleExpr e2)
simplifyBoolExpr (IntGreaterOrEqualFunc e1 e2) = IntGreaterOrEqualFunc (simplifyIntExpr e1) (simplifyIntExpr e2)
simplifyBoolExpr (UintGreaterOrEqualFunc e1 e2) = UintGreaterOrEqualFunc (simplifyUintExpr e1) (simplifyUintExpr e2)

simplifyBoolExpr (BytesGreaterThanFunc e1 e2) = BytesGreaterThanFunc (simplifyBytesExpr e1) (simplifyBytesExpr e2)
simplifyBoolExpr (DoubleGreaterThanFunc e1 e2) = DoubleGreaterThanFunc (simplifyDoubleExpr e1) (simplifyDoubleExpr e2)
simplifyBoolExpr (IntGreaterThanFunc e1 e2) = IntGreaterThanFunc (simplifyIntExpr e1) (simplifyIntExpr e2)
simplifyBoolExpr (UintGreaterThanFunc e1 e2) = UintGreaterThanFunc (simplifyUintExpr e1) (simplifyUintExpr e2)

simplifyBoolExpr (StringHasPrefixFunc e1 e2) = StringHasPrefixFunc (simplifyStringExpr e1) (simplifyStringExpr e2)
simplifyBoolExpr (StringHasSuffixFunc e1 e2) = StringHasSuffixFunc (simplifyStringExpr e1) (simplifyStringExpr e2)

simplifyBoolExpr (BytesLessOrEqualFunc e1 e2) = BytesLessOrEqualFunc (simplifyBytesExpr e1) (simplifyBytesExpr e2)
simplifyBoolExpr (DoubleLessOrEqualFunc e1 e2) = DoubleLessOrEqualFunc (simplifyDoubleExpr e1) (simplifyDoubleExpr e2)
simplifyBoolExpr (IntLessOrEqualFunc e1 e2) = IntLessOrEqualFunc (simplifyIntExpr e1) (simplifyIntExpr e2)
simplifyBoolExpr (UintLessOrEqualFunc e1 e2) = UintLessOrEqualFunc (simplifyUintExpr e1) (simplifyUintExpr e2)

simplifyBoolExpr (BytesLessThanFunc e1 e2) = BytesLessThanFunc (simplifyBytesExpr e1) (simplifyBytesExpr e2)
simplifyBoolExpr (DoubleLessThanFunc e1 e2) = DoubleLessThanFunc (simplifyDoubleExpr e1) (simplifyDoubleExpr e2)
simplifyBoolExpr (IntLessThanFunc e1 e2) = IntLessThanFunc (simplifyIntExpr e1) (simplifyIntExpr e2)
simplifyBoolExpr (UintLessThanFunc e1 e2) = UintLessThanFunc (simplifyUintExpr e1) (simplifyUintExpr e2)

simplifyBoolExpr (BoolNotEqualFunc e1 e2) = BoolNotEqualFunc (simplifyBoolExpr e1) (simplifyBoolExpr e2)
simplifyBoolExpr (DoubleNotEqualFunc e1 e2) = DoubleNotEqualFunc (simplifyDoubleExpr e1) (simplifyDoubleExpr e2)
simplifyBoolExpr (IntNotEqualFunc e1 e2) = IntNotEqualFunc (simplifyIntExpr e1) (simplifyIntExpr e2)
simplifyBoolExpr (UintNotEqualFunc e1 e2) = UintNotEqualFunc (simplifyUintExpr e1) (simplifyUintExpr e2)
simplifyBoolExpr (StringNotEqualFunc e1 e2) = StringNotEqualFunc (simplifyStringExpr e1) (simplifyStringExpr e2)
simplifyBoolExpr (BytesNotEqualFunc e1 e2) = BytesNotEqualFunc (simplifyBytesExpr e1) (simplifyBytesExpr e2)

simplifyBoolExpr (BytesTypeFunc e) = BytesTypeFunc (simplifyBytesExpr e)
simplifyBoolExpr (BoolTypeFunc e) = BoolTypeFunc (simplifyBoolExpr e)
simplifyBoolExpr (DoubleTypeFunc e) = DoubleTypeFunc (simplifyDoubleExpr e)
simplifyBoolExpr (IntTypeFunc e) = IntTypeFunc (simplifyIntExpr e)
simplifyBoolExpr (UintTypeFunc e) = UintTypeFunc (simplifyUintExpr e)
simplifyBoolExpr (StringTypeFunc e) = StringTypeFunc (simplifyStringExpr e)

simplifyOrFunc :: BoolExpr -> BoolExpr -> BoolExpr
simplifyOrFunc true@(BoolConst True) _ = true
simplifyOrFunc _ true@(BoolConst True) = true
simplifyOrFunc (BoolConst False) v = v
simplifyOrFunc v (BoolConst False) = v
simplifyOrFunc v1 v2
	| v1 == v2  = v1
	| v1 == (simplifyNotFunc v2) = BoolConst True
	| (simplifyNotFunc v1) == v2 = BoolConst True
	| otherwise = OrFunc v1 v2

simplifyAndFunc :: BoolExpr -> BoolExpr -> BoolExpr
simplifyAndFunc (BoolConst True) v = v
simplifyAndFunc v (BoolConst True) = v
simplifyAndFunc false@(BoolConst False) _ = false
simplifyAndFunc _ false@(BoolConst False) = false

simplifyAndFunc v1@(StringEqualFunc s1 s2) v2@(StringEqualFunc s1' s2') = 
	case (s1, s2, s1', s2') of
	(c1@(StringConst _), StringVariable, c2@(StringConst _), StringVariable) -> if c1 == c2 then v1 else BoolConst False
	(c1@(StringConst _), StringVariable, StringVariable, c2@(StringConst _)) -> if c1 == c2 then v1 else BoolConst False
	(StringVariable, c1@(StringConst _), c2@(StringConst _), StringVariable) -> if c1 == c2 then v1 else BoolConst False
	(StringVariable, c1@(StringConst _), StringVariable, c2@(StringConst _)) -> if c1 == c2 then v1 else BoolConst False
simplifyAndFunc v1@(StringEqualFunc s1 s2) v2@(StringNotEqualFunc s1' s2') = 
	case (s1, s2, s1', s2') of
	(c1@(StringConst _), StringVariable, c2@(StringConst _), StringVariable) -> if c1 /= c2 then v1 else BoolConst False
	(c1@(StringConst _), StringVariable, StringVariable, c2@(StringConst _)) -> if c1 /= c2 then v1 else BoolConst False
	(StringVariable, c1@(StringConst _), c2@(StringConst _), StringVariable) -> if c1 /= c2 then v1 else BoolConst False
	(StringVariable, c1@(StringConst _), StringVariable, c2@(StringConst _)) -> if c1 /= c2 then v1 else BoolConst False
simplifyAndFunc v1@(StringNotEqualFunc s1 s2) v2@(StringEqualFunc s1' s2') = 
	case (s1, s2, s1', s2') of
	(c1@(StringConst _), StringVariable, c2@(StringConst _), StringVariable) -> if c1 /= c2 then v1 else BoolConst False
	(c1@(StringConst _), StringVariable, StringVariable, c2@(StringConst _)) -> if c1 /= c2 then v1 else BoolConst False
	(StringVariable, c1@(StringConst _), c2@(StringConst _), StringVariable) -> if c1 /= c2 then v1 else BoolConst False
	(StringVariable, c1@(StringConst _), StringVariable, c2@(StringConst _)) -> if c1 /= c2 then v1 else BoolConst False

simplifyAndFunc v1@(IntEqualFunc s1 s2) v2@(IntEqualFunc s1' s2') = 
	case (s1, s2, s1', s2') of
	(c1@(IntConst _), IntVariable, c2@(IntConst _), IntVariable) -> if c1 == c2 then v1 else BoolConst False
	(c1@(IntConst _), IntVariable, IntVariable, c2@(IntConst _)) -> if c1 == c2 then v1 else BoolConst False
	(IntVariable, c1@(IntConst _), c2@(IntConst _), IntVariable) -> if c1 == c2 then v1 else BoolConst False
	(IntVariable, c1@(IntConst _), IntVariable, c2@(IntConst _)) -> if c1 == c2 then v1 else BoolConst False
simplifyAndFunc v1@(IntEqualFunc s1 s2) v2@(IntNotEqualFunc s1' s2') = 
	case (s1, s2, s1', s2') of
	(c1@(IntConst _), IntVariable, c2@(IntConst _), IntVariable) -> if c1 /= c2 then v1 else BoolConst False
	(c1@(IntConst _), IntVariable, IntVariable, c2@(IntConst _)) -> if c1 /= c2 then v1 else BoolConst False
	(IntVariable, c1@(IntConst _), c2@(IntConst _), IntVariable) -> if c1 /= c2 then v1 else BoolConst False
	(IntVariable, c1@(IntConst _), IntVariable, c2@(IntConst _)) -> if c1 /= c2 then v1 else BoolConst False
simplifyAndFunc v1@(IntNotEqualFunc s1 s2) v2@(IntEqualFunc s1' s2') = 
	case (s1, s2, s1', s2') of
	(c1@(IntConst _), IntVariable, c2@(IntConst _), IntVariable) -> if c1 /= c2 then v1 else BoolConst False
	(c1@(IntConst _), IntVariable, IntVariable, c2@(IntConst _)) -> if c1 /= c2 then v1 else BoolConst False
	(IntVariable, c1@(IntConst _), c2@(IntConst _), IntVariable) -> if c1 /= c2 then v1 else BoolConst False
	(IntVariable, c1@(IntConst _), IntVariable, c2@(IntConst _)) -> if c1 /= c2 then v1 else BoolConst False

simplifyAndFunc v1@(UintEqualFunc s1 s2) v2@(UintEqualFunc s1' s2') = 
	case (s1, s2, s1', s2') of
	(c1@(UintConst _), UintVariable, c2@(UintConst _), UintVariable) -> if c1 == c2 then v1 else BoolConst False
	(c1@(UintConst _), UintVariable, UintVariable, c2@(UintConst _)) -> if c1 == c2 then v1 else BoolConst False
	(UintVariable, c1@(UintConst _), c2@(UintConst _), UintVariable) -> if c1 == c2 then v1 else BoolConst False
	(UintVariable, c1@(UintConst _), UintVariable, c2@(UintConst _)) -> if c1 == c2 then v1 else BoolConst False
simplifyAndFunc v1@(UintEqualFunc s1 s2) v2@(UintNotEqualFunc s1' s2') = 
	case (s1, s2, s1', s2') of
	(c1@(UintConst _), UintVariable, c2@(UintConst _), UintVariable) -> if c1 /= c2 then v1 else BoolConst False
	(c1@(UintConst _), UintVariable, UintVariable, c2@(UintConst _)) -> if c1 /= c2 then v1 else BoolConst False
	(UintVariable, c1@(UintConst _), c2@(UintConst _), UintVariable) -> if c1 /= c2 then v1 else BoolConst False
	(UintVariable, c1@(UintConst _), UintVariable, c2@(UintConst _)) -> if c1 /= c2 then v1 else BoolConst False
simplifyAndFunc v1@(UintNotEqualFunc s1 s2) v2@(UintEqualFunc s1' s2') = 
	case (s1, s2, s1', s2') of
	(c1@(UintConst _), UintVariable, c2@(UintConst _), UintVariable) -> if c1 /= c2 then v1 else BoolConst False
	(c1@(UintConst _), UintVariable, UintVariable, c2@(UintConst _)) -> if c1 /= c2 then v1 else BoolConst False
	(UintVariable, c1@(UintConst _), c2@(UintConst _), UintVariable) -> if c1 /= c2 then v1 else BoolConst False
	(UintVariable, c1@(UintConst _), UintVariable, c2@(UintConst _)) -> if c1 /= c2 then v1 else BoolConst False

simplifyAndFunc v1 v2
	| v1 == v2  = v1
	| v1 == (simplifyNotFunc v2) = BoolConst False
	| (simplifyNotFunc v1) == v2 = BoolConst False
	| otherwise = AndFunc v1 v2

simplifyNotFunc :: BoolExpr -> BoolExpr
simplifyNotFunc (NotFunc v) = v
simplifyNotFunc (BoolConst True) = (BoolConst False)
simplifyNotFunc (BoolConst False) = (BoolConst True)
simplifyNotFunc (AndFunc e1 e2) = simplifyOrFunc (simplifyNotFunc e1) (simplifyNotFunc e2)
simplifyNotFunc (OrFunc e1 e2) = simplifyAndFunc (simplifyNotFunc e1) (simplifyNotFunc e2)
simplifyNotFunc v@(BoolEqualFunc e1 e2) = BoolNotEqualFunc e1 e2
simplifyNotFunc v@(DoubleEqualFunc e1 e2) = DoubleNotEqualFunc e1 e2
simplifyNotFunc v@(IntEqualFunc e1 e2) = IntNotEqualFunc e1 e2
simplifyNotFunc v@(UintEqualFunc e1 e2) = UintNotEqualFunc e1 e2
simplifyNotFunc v@(StringEqualFunc e1 e2) = StringNotEqualFunc e1 e2
simplifyNotFunc v@(BytesEqualFunc e1 e2) = BytesNotEqualFunc e1 e2
simplifyNotFunc v@(BoolNotEqualFunc e1 e2) = BoolEqualFunc e1 e2
simplifyNotFunc v@(DoubleNotEqualFunc e1 e2) = DoubleEqualFunc e1 e2
simplifyNotFunc v@(IntNotEqualFunc e1 e2) = IntEqualFunc e1 e2
simplifyNotFunc v@(UintNotEqualFunc e1 e2) = UintEqualFunc e1 e2
simplifyNotFunc v@(StringNotEqualFunc e1 e2) = StringEqualFunc e1 e2
simplifyNotFunc v@(BytesNotEqualFunc e1 e2) = BytesEqualFunc e1 e2
simplifyNotFunc v = NotFunc v

simplifyDoubleExpr :: DoubleExpr -> DoubleExpr
simplifyDoubleExpr (DoubleListElemFunc es e) = DoubleListElemFunc (map simplifyDoubleExpr es) (simplifyIntExpr e)
simplifyDoubleExpr e = e

simplifyIntExpr :: IntExpr -> IntExpr
simplifyIntExpr (IntListElemFunc es e) = IntListElemFunc (map simplifyIntExpr es) (simplifyIntExpr e)
simplifyIntExpr (BytesListLengthFunc es) = IntConst (length es)
simplifyIntExpr (BoolListLengthFunc es) = IntConst (length es)
simplifyIntExpr (BytesLengthFunc e) = case simplifyBytesExpr e of
		(BytesConst b) -> IntConst (length b)
		b -> BytesLengthFunc b
simplifyIntExpr (DoubleListLengthFunc es) = IntConst (length es)
simplifyIntExpr (IntListLengthFunc es) = IntConst (length es)
simplifyIntExpr (StringListLengthFunc es) = IntConst (length es)
simplifyIntExpr (UintListLengthFunc es) = IntConst (length es)
simplifyIntExpr (StringLengthFunc e) = case simplifyStringExpr e of
		(StringConst b) -> IntConst (length b)
		b -> StringLengthFunc b
simplifyIntExpr e = e

simplifyUintExpr :: UintExpr -> UintExpr
simplifyUintExpr (UintListElemFunc es e) = UintListElemFunc (map simplifyUintExpr es) (simplifyIntExpr e)
simplifyUintExpr e = e

simplifyStringExpr :: StringExpr -> StringExpr
simplifyStringExpr (StringListElemFunc es e) = StringListElemFunc (map simplifyStringExpr es) (simplifyIntExpr e)
simplifyStringExpr (StringToLowerFunc e) = case simplifyStringExpr e of
		(StringConst s) -> StringConst $ map toLower s
		s -> s
simplifyStringExpr (StringToUpperFunc e) = case simplifyStringExpr e of
		(StringConst s) -> StringConst $ map toUpper s
		s -> s
simplifyStringExpr e = e

simplifyBytesExpr :: BytesExpr -> BytesExpr
simplifyBytesExpr (BytesListElemFunc es e) = BytesListElemFunc (map simplifyBytesExpr es) (simplifyIntExpr e)
simplifyBytesExpr b = b