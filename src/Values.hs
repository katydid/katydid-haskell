module Values where

import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Char (toLower, toUpper)
import Text.Regex.TDFA
import Control.Monad.Except (Except, runExcept, throwError)

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

	| RegexFunc StringExpr StringExpr
	deriving (Eq, Ord, Show)

data DoubleExpr
	= DoubleConst Double
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

data ValueErr
	= ErrNotABool String
	| ErrNotAString String
	| ErrNotAnInt String
	| ErrNotADouble String
	| ErrNotAnUint String
	| ErrNotBytes String
	deriving (Eq, Ord, Show)

eval :: BoolExpr -> Label -> Except ValueErr Bool
eval = evalBool

must :: (Show e) => Except e v -> v
must ex = case runExcept ex of
	(Left e) -> error $ show e
	(Right v) -> v

evalBool :: BoolExpr -> Label -> Except ValueErr Bool

evalBool (BoolConst b) _ = return b
evalBool BoolVariable (Bool b) = return b
evalBool BoolVariable l = throwError $ ErrNotABool $ show l

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
evalBool (NotFunc e) v = case runExcept $ evalBool e v of
	(Right True) -> return False
	_ -> return True

evalBool (BoolEqualFunc e1 e2) v = eq (runExcept $ evalBool e1 v) (runExcept $ evalBool e2 v)
evalBool (DoubleEqualFunc e1 e2) v = eq (runExcept $ evalDouble e1 v) (runExcept $ evalDouble e2 v)
evalBool (IntEqualFunc e1 e2) v = eq (runExcept $ evalInt e1 v) (runExcept $ evalInt e2 v)
evalBool (UintEqualFunc e1 e2) v = eq (runExcept $ evalUint e1 v) (runExcept $ evalUint e2 v)
evalBool (StringEqualFunc e1 e2) v = eq (runExcept $ evalString e1 v) (runExcept $ evalString e2 v)
evalBool (BytesEqualFunc e1 e2) v = eq (runExcept $ evalBytes e1 v) (runExcept $ evalBytes e2 v)

evalBool (IntListContainsFunc e es) v = do {
	e' <- evalInt e v;
	es' <- mapM ((flip evalInt) v) es;
	return $ elem e' es'
}
evalBool (StringListContainsFunc e es) v = do {
	e' <- evalString e v;
	es' <- mapM ((flip evalString) v) es;
	return $ elem e' es'
}
evalBool (UintListContainsFunc e es) v = do {
	e' <- evalUint e v;
	es' <- mapM ((flip evalUint) v) es;
	return $ elem e' es'
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

evalBool (DoubleGreaterOrEqualFunc e1 e2) v = ge (runExcept $ evalDouble e1 v) (runExcept $ evalDouble e2 v)
evalBool (IntGreaterOrEqualFunc e1 e2) v = ge (runExcept $ evalInt e1 v) (runExcept $ evalInt e2 v)
evalBool (UintGreaterOrEqualFunc e1 e2) v = ge (runExcept $ evalUint e1 v) (runExcept $ evalUint e2 v)
evalBool (BytesGreaterOrEqualFunc e1 e2) v = ge (runExcept $ evalBytes e1 v) (runExcept $ evalBytes e2 v)

evalBool (DoubleGreaterThanFunc e1 e2) v = gt (runExcept $ evalDouble e1 v) (runExcept $ evalDouble e2 v)
evalBool (IntGreaterThanFunc e1 e2) v = gt (runExcept $ evalInt e1 v) (runExcept $ evalInt e2 v)
evalBool (UintGreaterThanFunc e1 e2) v = gt (runExcept $ evalUint e1 v) (runExcept $ evalUint e2 v)
evalBool (BytesGreaterThanFunc e1 e2) v = gt (runExcept $ evalBytes e1 v) (runExcept $ evalBytes e2 v)

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

evalBool (DoubleLessOrEqualFunc e1 e2) v = le (runExcept $ evalDouble e1 v) (runExcept $ evalDouble e2 v)
evalBool (IntLessOrEqualFunc e1 e2) v = le (runExcept $ evalInt e1 v) (runExcept $ evalInt e2 v)
evalBool (UintLessOrEqualFunc e1 e2) v = le (runExcept $ evalUint e1 v) (runExcept $ evalUint e2 v)
evalBool (BytesLessOrEqualFunc e1 e2) v = le (runExcept $ evalBytes e1 v) (runExcept $ evalBytes e2 v)

evalBool (DoubleLessThanFunc e1 e2) v = lt (runExcept $ evalDouble e1 v) (runExcept $ evalDouble e2 v)
evalBool (IntLessThanFunc e1 e2) v = lt (runExcept $ evalInt e1 v) (runExcept $ evalInt e2 v)
evalBool (UintLessThanFunc e1 e2) v = lt (runExcept $ evalUint e1 v) (runExcept $ evalUint e2 v)
evalBool (BytesLessThanFunc e1 e2) v = lt (runExcept $ evalBytes e1 v) (runExcept $ evalBytes e2 v)

evalBool (BoolNotEqualFunc e1 e2) v = ne (runExcept $ evalBool e1 v) (runExcept $ evalBool e2 v)
evalBool (DoubleNotEqualFunc e1 e2) v = ne (runExcept $ evalDouble e1 v) (runExcept $ evalDouble e2 v)
evalBool (IntNotEqualFunc e1 e2) v = ne (runExcept $ evalInt e1 v) (runExcept $ evalInt e2 v)
evalBool (UintNotEqualFunc e1 e2) v = ne (runExcept $ evalUint e1 v) (runExcept $ evalUint e2 v)
evalBool (StringNotEqualFunc e1 e2) v = ne (runExcept $ evalString e1 v) (runExcept $ evalString e2 v)
evalBool (BytesNotEqualFunc e1 e2) v = ne (runExcept $ evalBytes e1 v) (runExcept $ evalBytes e2 v)

evalBool (BytesTypeFunc e) v = case runExcept $ evalBytes e v of
	(Right _) -> return True
	(Left _) -> return False
evalBool (BoolTypeFunc e) v = case runExcept $ evalBool e v of
	(Right _) -> return True
	(Left _) -> return False
evalBool (DoubleTypeFunc e) v = case runExcept $ evalDouble e v of
	(Right _) -> return True
	(Left _) -> return False
evalBool (IntTypeFunc e) v = case runExcept $ evalInt e v of
	(Right _) -> return True
	(Left _) -> return False
evalBool (UintTypeFunc e) v = case runExcept $ evalUint e v of
	(Right _) -> return True
	(Left _) -> return False
evalBool (StringTypeFunc e) v = case runExcept $ evalString e v of
	(Right _) -> return True
	(Left _) -> return False

evalBool (RegexFunc e s) v = do {
	e' <- evalString e v;
	s' <- evalString s v;
	return (s' =~ e' :: Bool)
}

eq :: (Eq a) => (Either ValueErr a) -> (Either ValueErr a) -> Except ValueErr Bool
eq (Right v1) (Right v2) = return $ v1 == v2
eq (Left _) _ = return False
eq _ (Left _) = return False

ge :: (Ord a) => (Either ValueErr a) -> (Either ValueErr a) -> Except ValueErr Bool
ge (Right v1) (Right v2) = return $ v1 >= v2
ge (Left _) _ = return False
ge _ (Left _) = return False

gt :: (Ord a) => (Either ValueErr a) -> (Either ValueErr a) -> Except ValueErr Bool
gt (Right v1) (Right v2) = return $ v1 > v2
gt (Left _) _ = return False
gt _ (Left _) = return False

le :: (Ord a) => (Either ValueErr a) -> (Either ValueErr a) -> Except ValueErr Bool
le (Right v1) (Right v2) = return $ v1 <= v2
le (Left _) _ = return False
le _ (Left _) = return False

lt :: (Ord a) => (Either ValueErr a) -> (Either ValueErr a) -> Except ValueErr Bool
lt (Right v1) (Right v2) = return $ v1 < v2
lt (Left _) _ = return False
lt _ (Left _) = return False

ne :: (Eq a) => (Either ValueErr a) -> (Either ValueErr a) -> Except ValueErr Bool
ne (Right v1) (Right v2) = return $ v1 /= v2
ne (Left _) _ = return False
ne _ (Left _) = return False

evalDouble :: DoubleExpr -> Label -> Except ValueErr Double
evalDouble (DoubleConst r) _ = return r
evalDouble DoubleVariable (Number r) = return $ fromRational r
evalDouble DoubleVariable l = throwError $ ErrNotADouble $ show l

evalDouble (DoubleListElemFunc es i) v = do {
	i' <- evalInt i v;
	es' <- mapM ((flip evalDouble) v) es;
	return $ es' !! i'
}

evalInt :: IntExpr -> Label -> Except ValueErr Int
evalInt (IntConst i) _ = return i
evalInt IntVariable (Number r) = return (truncate r)
evalInt IntVariable l = throwError $ ErrNotAnInt $ show l

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

evalUint :: UintExpr -> Label -> Except ValueErr Int
evalUint (UintConst i) _ = return i
evalUint UintVariable (Number r) = return $ truncate r
evalUint UintVariable l = throwError $ ErrNotAnUint $ show l

evalUint (UintListElemFunc es i) v = do {
	i' <- evalInt i v;
	es' <- mapM ((flip evalUint) v) es;
	return $ es' !! i'
}

evalString :: StringExpr -> Label -> Except ValueErr String
evalString (StringConst i) _ = return i
evalString StringVariable (String s) = return s
evalString StringVariable l = throwError $ ErrNotAString $ show l

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

evalBytes :: BytesExpr -> Label -> Except ValueErr String
evalBytes (BytesConst u) _ = return u
evalBytes BytesVariable (String s) = return s
evalBytes BytesVariable l = throwError $ ErrNotBytes $ show l

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

simplifyBoolExpr (RegexFunc e1 e2) = RegexFunc (simplifyStringExpr e1) (simplifyStringExpr e2)

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