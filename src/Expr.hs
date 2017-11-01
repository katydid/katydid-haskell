{-#LANGUAGE GADTs, StandaloneDeriving #-}

-- |
-- This module contains all the Relapse expressions.
-- 
-- It also contains an eval function and a simplfication function for these expressions.
module Expr (
    -- * Expressions
    Expr(..), Bytes, Uint,
    -- * Functions
    simplifyBoolExpr, eval,
    -- * Errors
    ValueErr
) where

import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Char (toLower, toUpper)
import Text.Regex.TDFA ((=~))
import Control.Monad.Except (Except, runExcept, throwError)

import Parsers

type Bytes = String
type Uint = Int

data Expr a where
    -- Expr Bool

    BoolConst :: Bool -> Expr Bool
    BoolVariable :: Expr Bool

    OrFunc :: Expr Bool -> Expr Bool -> Expr Bool
    AndFunc :: Expr Bool -> Expr Bool -> Expr Bool
    NotFunc :: Expr Bool -> Expr Bool

    BoolEqualFunc :: Expr Bool -> Expr Bool -> Expr Bool
    DoubleEqualFunc :: Expr Double -> Expr Double -> Expr Bool
    IntEqualFunc :: Expr Int -> Expr Int -> Expr Bool
    UintEqualFunc :: Expr Uint -> Expr Uint -> Expr Bool
    StringEqualFunc :: Expr String -> Expr String -> Expr Bool
    BytesEqualFunc :: Expr Bytes -> Expr Bytes -> Expr Bool

    IntListContainsFunc :: Expr Int -> [Expr Int] -> Expr Bool
    StringListContainsFunc :: Expr String -> [Expr String] -> Expr Bool
    UintListContainsFunc :: Expr Uint -> [Expr Uint] -> Expr Bool
    StringContainsFunc :: Expr String -> Expr String -> Expr Bool

    BoolListElemFunc :: [Expr Bool] -> Expr Int -> Expr Bool

    BytesGreaterOrEqualFunc :: Expr Bytes -> Expr Bytes -> Expr Bool
    DoubleGreaterOrEqualFunc :: Expr Double -> Expr Double -> Expr Bool
    IntGreaterOrEqualFunc :: Expr Int -> Expr Int -> Expr Bool
    UintGreaterOrEqualFunc :: Expr Uint -> Expr Uint -> Expr Bool

    BytesGreaterThanFunc :: Expr Bytes -> Expr Bytes -> Expr Bool
    DoubleGreaterThanFunc :: Expr Double -> Expr Double -> Expr Bool
    IntGreaterThanFunc :: Expr Int -> Expr Int -> Expr Bool
    UintGreaterThanFunc :: Expr Uint -> Expr Uint -> Expr Bool

    StringHasPrefixFunc :: Expr String -> Expr String -> Expr Bool
    StringHasSuffixFunc :: Expr String -> Expr String -> Expr Bool

    BytesLessOrEqualFunc :: Expr Bytes -> Expr Bytes -> Expr Bool
    DoubleLessOrEqualFunc :: Expr Double -> Expr Double -> Expr Bool
    IntLessOrEqualFunc :: Expr Int -> Expr Int -> Expr Bool
    UintLessOrEqualFunc :: Expr Uint -> Expr Uint -> Expr Bool

    BytesLessThanFunc :: Expr Bytes -> Expr Bytes -> Expr Bool
    DoubleLessThanFunc :: Expr Double -> Expr Double -> Expr Bool
    IntLessThanFunc :: Expr Int -> Expr Int -> Expr Bool
    UintLessThanFunc :: Expr Uint -> Expr Uint -> Expr Bool

    BytesNotEqualFunc :: Expr Bytes -> Expr Bytes -> Expr Bool
    BoolNotEqualFunc :: Expr Bool -> Expr Bool -> Expr Bool
    DoubleNotEqualFunc :: Expr Double -> Expr Double -> Expr Bool
    IntNotEqualFunc :: Expr Int -> Expr Int -> Expr Bool
    StringNotEqualFunc :: Expr String -> Expr String -> Expr Bool
    UintNotEqualFunc :: Expr Uint -> Expr Uint -> Expr Bool

    BytesTypeFunc :: Expr Bytes -> Expr Bool
    BoolTypeFunc :: Expr Bool -> Expr Bool
    DoubleTypeFunc :: Expr Double -> Expr Bool
    IntTypeFunc :: Expr Int -> Expr Bool
    UintTypeFunc :: Expr Uint -> Expr Bool
    StringTypeFunc :: Expr String -> Expr Bool

    RegexFunc :: Expr String -> Expr String -> Expr Bool

    -- Expr Double

    DoubleConst :: Double -> Expr Double
    DoubleVariable :: Expr Double

    DoubleListElemFunc :: [Expr Double] -> Expr Int -> Expr Double

    -- Expr Int

    IntConst :: Int -> Expr Int
    IntVariable :: Expr Int

    IntListElemFunc :: [Expr Int] -> Expr Int -> Expr Int

    BytesListLengthFunc :: [Expr Bytes] -> Expr Int
    BoolListLengthFunc :: [Expr Bool] -> Expr Int
    BytesLengthFunc :: Expr Bytes -> Expr Int
    DoubleListLengthFunc :: [Expr Double] -> Expr Int
    IntListLengthFunc :: [Expr Int] -> Expr Int
    StringListLengthFunc :: [Expr String] -> Expr Int
    UintListLengthFunc :: [Expr Uint] -> Expr Int
    StringLengthFunc :: Expr String -> Expr Int

    -- Expr Uint

    UintConst :: Uint -> Expr Uint
    UintVariable :: Expr Uint

    UintListElemFunc :: [Expr Uint] -> Expr Int -> Expr Uint

    -- Expr String

    StringConst :: String -> Expr String
    StringVariable :: Expr String
    StringListElemFunc :: [Expr String] -> Expr Int -> Expr String
    StringToLowerFunc :: Expr String -> Expr String
    StringToUpperFunc :: Expr String -> Expr String

    -- Expr Bytes

    BytesConst :: Bytes -> Expr Bytes
    BytesVariable :: Expr Bytes
    
    BytesListElemFunc :: [Expr Bytes] -> Expr Int -> Expr Bytes

deriving instance Eq a => Eq (Expr a)
deriving instance Ord a => Ord (Expr a)
deriving instance Show a => Show (Expr a)

data ValueErr
    = ErrNotABool String
    | ErrNotAString String
    | ErrNotAnInt String
    | ErrNotADouble String
    | ErrNotAnUint String
    | ErrNotBytes String
    deriving (Eq, Ord, Show)

-- |
-- eval evaluates a boolean expression, given an input label.
eval :: Expr Bool -> Label -> Except ValueErr Bool
eval = evalBool

evalBool :: Expr Bool -> Label -> Except ValueErr Bool

evalBool (BoolConst b) _ = return b
evalBool BoolVariable (Bool b) = return b
evalBool BoolVariable l = throwError $ ErrNotABool $ show l

evalBool (OrFunc e1 e2) v = (||) <$> evalBool e1 v <*> evalBool e2 v

evalBool (AndFunc e1 e2) v = (&&) <$> evalBool e1 v <*> evalBool e2 v

evalBool (NotFunc e) v = case runExcept $ evalBool e v of
    (Right True) -> return False
    _ -> return True

evalBool (BoolEqualFunc e1 e2) v = eq (runExcept $ evalBool e1 v) (runExcept $ evalBool e2 v)
evalBool (DoubleEqualFunc e1 e2) v = eq (runExcept $ evalDouble e1 v) (runExcept $ evalDouble e2 v)
evalBool (IntEqualFunc e1 e2) v = eq (runExcept $ evalInt e1 v) (runExcept $ evalInt e2 v)
evalBool (UintEqualFunc e1 e2) v = eq (runExcept $ evalUint e1 v) (runExcept $ evalUint e2 v)
evalBool (StringEqualFunc e1 e2) v = eq (runExcept $ evalString e1 v) (runExcept $ evalString e2 v)
evalBool (BytesEqualFunc e1 e2) v = eq (runExcept $ evalBytes e1 v) (runExcept $ evalBytes e2 v)

evalBool (IntListContainsFunc e es) v = elem <$> evalInt e v <*> mapM (`evalInt` v) es

evalBool (StringListContainsFunc e es) v = elem <$> evalString e v <*> mapM (`evalString` v) es

evalBool (UintListContainsFunc e es) v = elem <$> evalUint e v <*> mapM (`evalUint` v) es

evalBool (StringContainsFunc s sub) v = isInfixOf <$> evalString sub v <*> evalString s v

evalBool (BoolListElemFunc es i) v =
    (!!) <$>
        mapM (`evalBool` v) es <*>
        evalInt i v

evalBool (DoubleGreaterOrEqualFunc e1 e2) v = ge (runExcept $ evalDouble e1 v) (runExcept $ evalDouble e2 v)
evalBool (IntGreaterOrEqualFunc e1 e2) v = ge (runExcept $ evalInt e1 v) (runExcept $ evalInt e2 v)
evalBool (UintGreaterOrEqualFunc e1 e2) v = ge (runExcept $ evalUint e1 v) (runExcept $ evalUint e2 v)
evalBool (BytesGreaterOrEqualFunc e1 e2) v = ge (runExcept $ evalBytes e1 v) (runExcept $ evalBytes e2 v)

evalBool (DoubleGreaterThanFunc e1 e2) v = gt (runExcept $ evalDouble e1 v) (runExcept $ evalDouble e2 v)
evalBool (IntGreaterThanFunc e1 e2) v = gt (runExcept $ evalInt e1 v) (runExcept $ evalInt e2 v)
evalBool (UintGreaterThanFunc e1 e2) v = gt (runExcept $ evalUint e1 v) (runExcept $ evalUint e2 v)
evalBool (BytesGreaterThanFunc e1 e2) v = gt (runExcept $ evalBytes e1 v) (runExcept $ evalBytes e2 v)

evalBool (StringHasPrefixFunc e1 e2) v = isPrefixOf <$> evalString e2 v <*> evalString e1 v

evalBool (StringHasSuffixFunc e1 e2) v = isSuffixOf <$> evalString e2 v <*> evalString e1 v

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

evalBool (RegexFunc e s) v = (=~) <$> evalString s v <*> evalString e v

eq :: (Eq a) => Either ValueErr a -> Either ValueErr a -> Except ValueErr Bool
eq (Right v1) (Right v2) = return $ v1 == v2
eq (Left _) _ = return False
eq _ (Left _) = return False

ge :: (Ord a) => Either ValueErr a -> Either ValueErr a -> Except ValueErr Bool
ge (Right v1) (Right v2) = return $ v1 >= v2
ge (Left _) _ = return False
ge _ (Left _) = return False

gt :: (Ord a) => Either ValueErr a -> Either ValueErr a -> Except ValueErr Bool
gt (Right v1) (Right v2) = return $ v1 > v2
gt (Left _) _ = return False
gt _ (Left _) = return False

le :: (Ord a) => Either ValueErr a -> Either ValueErr a -> Except ValueErr Bool
le (Right v1) (Right v2) = return $ v1 <= v2
le (Left _) _ = return False
le _ (Left _) = return False

lt :: (Ord a) => Either ValueErr a -> Either ValueErr a -> Except ValueErr Bool
lt (Right v1) (Right v2) = return $ v1 < v2
lt (Left _) _ = return False
lt _ (Left _) = return False

ne :: (Eq a) => Either ValueErr a -> Either ValueErr a -> Except ValueErr Bool
ne (Right v1) (Right v2) = return $ v1 /= v2
ne (Left _) _ = return False
ne _ (Left _) = return False

evalDouble :: Expr Double -> Label -> Except ValueErr Double
evalDouble (DoubleConst r) _ = return r
evalDouble DoubleVariable (Number r) = return $ fromRational r
evalDouble DoubleVariable l = throwError $ ErrNotADouble $ show l

evalDouble (DoubleListElemFunc es i) v = 
    (!!) <$> 
        mapM (`evalDouble` v) es <*> 
        evalInt i v

evalInt :: Expr Int -> Label -> Except ValueErr Int
evalInt (IntConst i) _ = return i
evalInt IntVariable (Number r) = return (truncate r)
evalInt IntVariable l = throwError $ ErrNotAnInt $ show l

evalInt (IntListElemFunc es i) v =
    (!!) <$>
        mapM (`evalInt` v) es <*>
        evalInt i v

evalInt (BytesListLengthFunc es) v = length <$> mapM (`evalBytes` v) es

evalInt (BoolListLengthFunc es) v = length <$> mapM (`evalBool` v) es

evalInt (BytesLengthFunc e) v = length <$> evalBytes e v

evalInt (DoubleListLengthFunc es) v = length <$> mapM (`evalDouble` v) es

evalInt (IntListLengthFunc es) v = length <$> mapM (`evalInt` v) es

evalInt (StringListLengthFunc es) v = length <$> mapM (`evalString` v) es

evalInt (UintListLengthFunc es) v = length <$> mapM (`evalUint` v) es

evalInt (StringLengthFunc e) v = length <$> evalString e v

evalUint :: Expr Uint -> Label -> Except ValueErr Int
evalUint (UintConst i) _ = return i
evalUint UintVariable (Number r) = return $ truncate r
evalUint UintVariable l = throwError $ ErrNotAnUint $ show l

evalUint (UintListElemFunc es i) v =
    (!!) <$>
        mapM (`evalUint` v) es <*>
        evalInt i v

evalString :: Expr String -> Label -> Except ValueErr String
evalString (StringConst i) _ = return i
evalString StringVariable (String s) = return s
evalString StringVariable l = throwError $ ErrNotAString $ show l

evalString (StringListElemFunc es i) v =
    (!!) <$>
        mapM (`evalString` v) es <*>
        evalInt i v

evalString (StringToLowerFunc s) v = map toLower <$> evalString s v

evalString (StringToUpperFunc s) v = map toUpper <$> evalString s v

evalBytes :: Expr Bytes -> Label -> Except ValueErr String
evalBytes (BytesConst u) _ = return u
evalBytes BytesVariable (String s) = return s
evalBytes BytesVariable l = throwError $ ErrNotBytes $ show l

evalBytes (BytesListElemFunc es i) v =
    (!!) <$>
        mapM (`evalBytes` v) es <*>
        evalInt i v

-- |
-- simplifyBoolExpr returns an equivalent, but simpler version of the input boolean expression.
simplifyBoolExpr :: Expr Bool -> Expr Bool
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

simplifyOrFunc :: Expr Bool -> Expr Bool -> Expr Bool
simplifyOrFunc true@(BoolConst True) _ = true
simplifyOrFunc _ true@(BoolConst True) = true
simplifyOrFunc (BoolConst False) v = v
simplifyOrFunc v (BoolConst False) = v
simplifyOrFunc v1 v2
    | v1 == v2  = v1
    | v1 == simplifyNotFunc v2 = BoolConst True
    | simplifyNotFunc v1 == v2 = BoolConst True
    | otherwise = OrFunc v1 v2

simplifyAndFunc :: Expr Bool -> Expr Bool -> Expr Bool
simplifyAndFunc (BoolConst True) v = v
simplifyAndFunc v (BoolConst True) = v
simplifyAndFunc false@(BoolConst False) _ = false
simplifyAndFunc _ false@(BoolConst False) = false

simplifyAndFunc v1@(StringEqualFunc s1 s2) v2@(StringEqualFunc s1' s2') = 
    case (s1, s2, s1', s2') of
    ((StringConst c1), StringVariable, (StringConst c2), StringVariable) -> if c1 == c2 then v1 else BoolConst False
    ((StringConst c1), StringVariable, StringVariable, (StringConst c2)) -> if c1 == c2 then v1 else BoolConst False
    (StringVariable, (StringConst c1), (StringConst c2), StringVariable) -> if c1 == c2 then v1 else BoolConst False
    (StringVariable, (StringConst c1), StringVariable, (StringConst c2)) -> if c1 == c2 then v1 else BoolConst False
simplifyAndFunc v1@(StringEqualFunc s1 s2) v2@(StringNotEqualFunc s1' s2') = 
    case (s1, s2, s1', s2') of
    ((StringConst c1), StringVariable, (StringConst c2), StringVariable) -> if c1 /= c2 then v1 else BoolConst False
    ((StringConst c1), StringVariable, StringVariable, (StringConst c2)) -> if c1 /= c2 then v1 else BoolConst False
    (StringVariable, (StringConst c1), (StringConst c2), StringVariable) -> if c1 /= c2 then v1 else BoolConst False
    (StringVariable, (StringConst c1), StringVariable, (StringConst c2)) -> if c1 /= c2 then v1 else BoolConst False
simplifyAndFunc v1@(StringNotEqualFunc s1 s2) v2@(StringEqualFunc s1' s2') = 
    case (s1, s2, s1', s2') of
    ((StringConst c1), StringVariable, (StringConst c2), StringVariable) -> if c1 /= c2 then v1 else BoolConst False
    ((StringConst c1), StringVariable, StringVariable, (StringConst c2)) -> if c1 /= c2 then v1 else BoolConst False
    (StringVariable, (StringConst c1), (StringConst c2), StringVariable) -> if c1 /= c2 then v1 else BoolConst False
    (StringVariable, (StringConst c1), StringVariable, (StringConst c2)) -> if c1 /= c2 then v1 else BoolConst False

simplifyAndFunc v1@(IntEqualFunc s1 s2) v2@(IntEqualFunc s1' s2') = 
    case (s1, s2, s1', s2') of
    ((IntConst c1), IntVariable, (IntConst c2), IntVariable) -> if c1 == c2 then v1 else BoolConst False
    ((IntConst c1), IntVariable, IntVariable, (IntConst c2)) -> if c1 == c2 then v1 else BoolConst False
    (IntVariable, (IntConst c1), (IntConst c2), IntVariable) -> if c1 == c2 then v1 else BoolConst False
    (IntVariable, (IntConst c1), IntVariable, (IntConst c2)) -> if c1 == c2 then v1 else BoolConst False
simplifyAndFunc v1@(IntEqualFunc s1 s2) v2@(IntNotEqualFunc s1' s2') = 
    case (s1, s2, s1', s2') of
    ((IntConst c1), IntVariable, (IntConst c2), IntVariable) -> if c1 /= c2 then v1 else BoolConst False
    ((IntConst c1), IntVariable, IntVariable, (IntConst c2)) -> if c1 /= c2 then v1 else BoolConst False
    (IntVariable, (IntConst c1), (IntConst c2), IntVariable) -> if c1 /= c2 then v1 else BoolConst False
    (IntVariable, (IntConst c1), IntVariable, (IntConst c2)) -> if c1 /= c2 then v1 else BoolConst False
simplifyAndFunc v1@(IntNotEqualFunc s1 s2) v2@(IntEqualFunc s1' s2') = 
    case (s1, s2, s1', s2') of
    ((IntConst c1), IntVariable, (IntConst c2), IntVariable) -> if c1 /= c2 then v1 else BoolConst False
    ((IntConst c1), IntVariable, IntVariable, (IntConst c2)) -> if c1 /= c2 then v1 else BoolConst False
    (IntVariable, (IntConst c1), (IntConst c2), IntVariable) -> if c1 /= c2 then v1 else BoolConst False
    (IntVariable, (IntConst c1), IntVariable, (IntConst c2)) -> if c1 /= c2 then v1 else BoolConst False

simplifyAndFunc v1@(UintEqualFunc s1 s2) v2@(UintEqualFunc s1' s2') = 
    case (s1, s2, s1', s2') of
    ((UintConst c1), UintVariable, (UintConst c2), UintVariable) -> if c1 == c2 then v1 else BoolConst False
    ((UintConst c1), UintVariable, UintVariable, (UintConst c2)) -> if c1 == c2 then v1 else BoolConst False
    (UintVariable, (UintConst c1), (UintConst c2), UintVariable) -> if c1 == c2 then v1 else BoolConst False
    (UintVariable, (UintConst c1), UintVariable, (UintConst c2)) -> if c1 == c2 then v1 else BoolConst False
simplifyAndFunc v1@(UintEqualFunc s1 s2) v2@(UintNotEqualFunc s1' s2') = 
    case (s1, s2, s1', s2') of
    ((UintConst c1), UintVariable, (UintConst c2), UintVariable) -> if c1 /= c2 then v1 else BoolConst False
    ((UintConst c1), UintVariable, UintVariable, (UintConst c2)) -> if c1 /= c2 then v1 else BoolConst False
    (UintVariable, (UintConst c1), (UintConst c2), UintVariable) -> if c1 /= c2 then v1 else BoolConst False
    (UintVariable, (UintConst c1), UintVariable, (UintConst c2)) -> if c1 /= c2 then v1 else BoolConst False
simplifyAndFunc v1@(UintNotEqualFunc s1 s2) v2@(UintEqualFunc s1' s2') = 
    case (s1, s2, s1', s2') of
    ((UintConst c1), UintVariable, (UintConst c2), UintVariable) -> if c1 /= c2 then v1 else BoolConst False
    ((UintConst c1), UintVariable, UintVariable, (UintConst c2)) -> if c1 /= c2 then v1 else BoolConst False
    (UintVariable, (UintConst c1), (UintConst c2), UintVariable) -> if c1 /= c2 then v1 else BoolConst False
    (UintVariable, (UintConst c1), UintVariable, (UintConst c2)) -> if c1 /= c2 then v1 else BoolConst False

simplifyAndFunc v1 v2
    | v1 == v2  = v1
    | v1 == simplifyNotFunc v2 = BoolConst False
    | simplifyNotFunc v1 == v2 = BoolConst False
    | otherwise = AndFunc v1 v2

simplifyNotFunc :: Expr Bool -> Expr Bool
simplifyNotFunc (NotFunc v) = v
simplifyNotFunc (BoolConst True) = BoolConst False
simplifyNotFunc (BoolConst False) = BoolConst True
simplifyNotFunc (AndFunc e1 e2) = simplifyOrFunc (simplifyNotFunc e1) (simplifyNotFunc e2)
simplifyNotFunc (OrFunc e1 e2) = simplifyAndFunc (simplifyNotFunc e1) (simplifyNotFunc e2)
simplifyNotFunc (BoolEqualFunc e1 e2) = BoolNotEqualFunc e1 e2
simplifyNotFunc (DoubleEqualFunc e1 e2) = DoubleNotEqualFunc e1 e2
simplifyNotFunc (IntEqualFunc e1 e2) = IntNotEqualFunc e1 e2
simplifyNotFunc (UintEqualFunc e1 e2) = UintNotEqualFunc e1 e2
simplifyNotFunc (StringEqualFunc e1 e2) = StringNotEqualFunc e1 e2
simplifyNotFunc (BytesEqualFunc e1 e2) = BytesNotEqualFunc e1 e2
simplifyNotFunc (BoolNotEqualFunc e1 e2) = BoolEqualFunc e1 e2
simplifyNotFunc (DoubleNotEqualFunc e1 e2) = DoubleEqualFunc e1 e2
simplifyNotFunc (IntNotEqualFunc e1 e2) = IntEqualFunc e1 e2
simplifyNotFunc (UintNotEqualFunc e1 e2) = UintEqualFunc e1 e2
simplifyNotFunc (StringNotEqualFunc e1 e2) = StringEqualFunc e1 e2
simplifyNotFunc (BytesNotEqualFunc e1 e2) = BytesEqualFunc e1 e2
simplifyNotFunc v = NotFunc v

simplifyDoubleExpr :: Expr Double -> Expr Double
simplifyDoubleExpr (DoubleListElemFunc es e) = DoubleListElemFunc (map simplifyDoubleExpr es) (simplifyIntExpr e)
simplifyDoubleExpr e = e

simplifyIntExpr :: Expr Int -> Expr Int
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

simplifyUintExpr :: Expr Uint -> Expr Uint
simplifyUintExpr (UintListElemFunc es e) = UintListElemFunc (map simplifyUintExpr es) (simplifyIntExpr e)
simplifyUintExpr e = e

simplifyStringExpr :: Expr String -> Expr String
simplifyStringExpr (StringListElemFunc es e) = StringListElemFunc (map simplifyStringExpr es) (simplifyIntExpr e)
simplifyStringExpr (StringToLowerFunc e) = case simplifyStringExpr e of
        (StringConst s) -> StringConst $ map toLower s
        s -> s
simplifyStringExpr (StringToUpperFunc e) = case simplifyStringExpr e of
        (StringConst s) -> StringConst $ map toUpper s
        s -> s
simplifyStringExpr e = e

simplifyBytesExpr :: Expr Bytes -> Expr Bytes
simplifyBytesExpr (BytesListElemFunc es e) = BytesListElemFunc (map simplifyBytesExpr es) (simplifyIntExpr e)
simplifyBytesExpr b = b