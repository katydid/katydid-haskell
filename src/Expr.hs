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

    Const :: a -> Expr a
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

    DoubleVariable :: Expr Double

    DoubleListElemFunc :: [Expr Double] -> Expr Int -> Expr Double

    -- Expr Int

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

    UintVariable :: Expr Uint

    UintListElemFunc :: [Expr Uint] -> Expr Int -> Expr Uint

    -- Expr String

    StringVariable :: Expr String
    StringListElemFunc :: [Expr String] -> Expr Int -> Expr String
    StringToLowerFunc :: Expr String -> Expr String
    StringToUpperFunc :: Expr String -> Expr String

    -- Expr Bytes

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
eval = ev

ev :: Expr a -> Label -> Except ValueErr a

ev (Const b) _ = return b
ev BoolVariable (Bool b) = return b
ev BoolVariable l = throwError $ ErrNotABool $ show l

ev (OrFunc e1 e2) v = (||) <$> ev e1 v <*> ev e2 v

ev (AndFunc e1 e2) v = (&&) <$> ev e1 v <*> ev e2 v

ev (NotFunc e) v = case runExcept $ ev e v of
    (Right True) -> return False
    _ -> return True

ev (BoolEqualFunc e1 e2) v = eq (runExcept $ ev e1 v) (runExcept $ ev e2 v)
ev (DoubleEqualFunc e1 e2) v = eq (runExcept $ ev e1 v) (runExcept $ ev e2 v)
ev (IntEqualFunc e1 e2) v = eq (runExcept $ ev e1 v) (runExcept $ ev e2 v)
ev (UintEqualFunc e1 e2) v = eq (runExcept $ ev e1 v) (runExcept $ ev e2 v)
ev (StringEqualFunc e1 e2) v = eq (runExcept $ ev e1 v) (runExcept $ ev e2 v)
ev (BytesEqualFunc e1 e2) v = eq (runExcept $ ev e1 v) (runExcept $ ev e2 v)

ev (IntListContainsFunc e es) v = elem <$> ev e v <*> mapM (`ev` v) es

ev (StringListContainsFunc e es) v = elem <$> ev e v <*> mapM (`ev` v) es

ev (UintListContainsFunc e es) v = elem <$> ev e v <*> mapM (`ev` v) es

ev (StringContainsFunc s sub) v = isInfixOf <$> ev sub v <*> ev s v

ev (BoolListElemFunc es i) v =
    (!!) <$>
        mapM (`ev` v) es <*>
        ev i v

ev (DoubleGreaterOrEqualFunc e1 e2) v = ge (runExcept $ ev e1 v) (runExcept $ ev e2 v)
ev (IntGreaterOrEqualFunc e1 e2) v = ge (runExcept $ ev e1 v) (runExcept $ ev e2 v)
ev (UintGreaterOrEqualFunc e1 e2) v = ge (runExcept $ ev e1 v) (runExcept $ ev e2 v)
ev (BytesGreaterOrEqualFunc e1 e2) v = ge (runExcept $ ev e1 v) (runExcept $ ev e2 v)

ev (DoubleGreaterThanFunc e1 e2) v = gt (runExcept $ ev e1 v) (runExcept $ ev e2 v)
ev (IntGreaterThanFunc e1 e2) v = gt (runExcept $ ev e1 v) (runExcept $ ev e2 v)
ev (UintGreaterThanFunc e1 e2) v = gt (runExcept $ ev e1 v) (runExcept $ ev e2 v)
ev (BytesGreaterThanFunc e1 e2) v = gt (runExcept $ ev e1 v) (runExcept $ ev e2 v)

ev (StringHasPrefixFunc e1 e2) v = isPrefixOf <$> ev e2 v <*> ev e1 v

ev (StringHasSuffixFunc e1 e2) v = isSuffixOf <$> ev e2 v <*> ev e1 v

ev (DoubleLessOrEqualFunc e1 e2) v = le (runExcept $ ev e1 v) (runExcept $ ev e2 v)
ev (IntLessOrEqualFunc e1 e2) v = le (runExcept $ ev e1 v) (runExcept $ ev e2 v)
ev (UintLessOrEqualFunc e1 e2) v = le (runExcept $ ev e1 v) (runExcept $ ev e2 v)
ev (BytesLessOrEqualFunc e1 e2) v = le (runExcept $ ev e1 v) (runExcept $ ev e2 v)

ev (DoubleLessThanFunc e1 e2) v = lt (runExcept $ ev e1 v) (runExcept $ ev e2 v)
ev (IntLessThanFunc e1 e2) v = lt (runExcept $ ev e1 v) (runExcept $ ev e2 v)
ev (UintLessThanFunc e1 e2) v = lt (runExcept $ ev e1 v) (runExcept $ ev e2 v)
ev (BytesLessThanFunc e1 e2) v = lt (runExcept $ ev e1 v) (runExcept $ ev e2 v)

ev (BoolNotEqualFunc e1 e2) v = ne (runExcept $ ev e1 v) (runExcept $ ev e2 v)
ev (DoubleNotEqualFunc e1 e2) v = ne (runExcept $ ev e1 v) (runExcept $ ev e2 v)
ev (IntNotEqualFunc e1 e2) v = ne (runExcept $ ev e1 v) (runExcept $ ev e2 v)
ev (UintNotEqualFunc e1 e2) v = ne (runExcept $ ev e1 v) (runExcept $ ev e2 v)
ev (StringNotEqualFunc e1 e2) v = ne (runExcept $ ev e1 v) (runExcept $ ev e2 v)
ev (BytesNotEqualFunc e1 e2) v = ne (runExcept $ ev e1 v) (runExcept $ ev e2 v)

ev (BytesTypeFunc e) v = case runExcept $ ev e v of
    (Right _) -> return True
    (Left _) -> return False
ev (BoolTypeFunc e) v = case runExcept $ ev e v of
    (Right _) -> return True
    (Left _) -> return False
ev (DoubleTypeFunc e) v = case runExcept $ ev e v of
    (Right _) -> return True
    (Left _) -> return False
ev (IntTypeFunc e) v = case runExcept $ ev e v of
    (Right _) -> return True
    (Left _) -> return False
ev (UintTypeFunc e) v = case runExcept $ ev e v of
    (Right _) -> return True
    (Left _) -> return False
ev (StringTypeFunc e) v = case runExcept $ ev e v of
    (Right _) -> return True
    (Left _) -> return False

ev (RegexFunc e s) v = (=~) <$> ev s v <*> ev e v

ev DoubleVariable (Number r) = return $ fromRational r
ev DoubleVariable l = throwError $ ErrNotADouble $ show l

ev (DoubleListElemFunc es i) v = 
    (!!) <$> 
        mapM (`ev` v) es <*> 
        ev i v

ev IntVariable (Number r) = return (truncate r)
ev IntVariable l = throwError $ ErrNotAnInt $ show l

ev (IntListElemFunc es i) v =
    (!!) <$>
        mapM (`ev` v) es <*>
        ev i v

ev (BytesListLengthFunc es) v = length <$> mapM (`ev` v) es

ev (BoolListLengthFunc es) v = length <$> mapM (`ev` v) es

ev (BytesLengthFunc e) v = length <$> ev e v

ev (DoubleListLengthFunc es) v = length <$> mapM (`ev` v) es

ev (IntListLengthFunc es) v = length <$> mapM (`ev` v) es

ev (StringListLengthFunc es) v = length <$> mapM (`ev` v) es

ev (UintListLengthFunc es) v = length <$> mapM (`ev` v) es

ev (StringLengthFunc e) v = length <$> ev e v

ev UintVariable (Number r) = return $ truncate r
ev UintVariable l = throwError $ ErrNotAnUint $ show l

ev (UintListElemFunc es i) v =
    (!!) <$>
        mapM (`ev` v) es <*>
        ev i v

ev StringVariable (String s) = return s
ev StringVariable l = throwError $ ErrNotAString $ show l

ev (StringListElemFunc es i) v =
    (!!) <$>
        mapM (`ev` v) es <*>
        ev i v

ev (StringToLowerFunc s) v = map toLower <$> ev s v

ev (StringToUpperFunc s) v = map toUpper <$> ev s v

ev BytesVariable (String s) = return s
ev BytesVariable l = throwError $ ErrNotBytes $ show l

ev (BytesListElemFunc es i) v =
    (!!) <$>
        mapM (`ev` v) es <*>
        ev i v

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

-- |
-- simplifyBoolExpr returns an equivalent, but simpler version of the input boolean expression.
simplifyBoolExpr :: Expr Bool -> Expr Bool
simplifyBoolExpr = simplifyExpr

simplifyExpr :: Expr a -> Expr a
simplifyExpr (BoolEqualFunc (Const b1) (Const b2)) = Const $ b1 == b2
simplifyExpr v@(Const _) = v
simplifyExpr v@BoolVariable = v

simplifyExpr (OrFunc v1 v2) = simplifyOrFunc (simplifyExpr v1) (simplifyExpr v2)
simplifyExpr (AndFunc v1 v2) = simplifyAndFunc (simplifyExpr v1) (simplifyExpr v2)
simplifyExpr (NotFunc v) = simplifyNotFunc (simplifyExpr v)

simplifyExpr (BoolEqualFunc e1 e2) = BoolEqualFunc (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr (DoubleEqualFunc e1 e2) = DoubleEqualFunc (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr (IntEqualFunc e1 e2) = IntEqualFunc (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr (UintEqualFunc e1 e2) = UintEqualFunc (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr (StringEqualFunc e1 e2) = StringEqualFunc (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr (BytesEqualFunc e1 e2) = BytesEqualFunc (simplifyExpr e1) (simplifyExpr e2)

simplifyExpr (IntListContainsFunc e es) = IntListContainsFunc (simplifyExpr e) (map simplifyExpr es)
simplifyExpr (StringListContainsFunc e es) = StringListContainsFunc (simplifyExpr e) (map simplifyExpr es)
simplifyExpr (UintListContainsFunc e es) = UintListContainsFunc (simplifyExpr e) (map simplifyExpr es)
simplifyExpr (StringContainsFunc e1 e2) = StringContainsFunc (simplifyExpr e1) (simplifyExpr e2)

simplifyExpr (BoolListElemFunc es e) = BoolListElemFunc (map simplifyExpr es) (simplifyExpr e)

simplifyExpr (BytesGreaterOrEqualFunc e1 e2) = BytesGreaterOrEqualFunc (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr (DoubleGreaterOrEqualFunc e1 e2) = DoubleGreaterOrEqualFunc (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr (IntGreaterOrEqualFunc e1 e2) = IntGreaterOrEqualFunc (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr (UintGreaterOrEqualFunc e1 e2) = UintGreaterOrEqualFunc (simplifyExpr e1) (simplifyExpr e2)

simplifyExpr (BytesGreaterThanFunc e1 e2) = BytesGreaterThanFunc (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr (DoubleGreaterThanFunc e1 e2) = DoubleGreaterThanFunc (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr (IntGreaterThanFunc e1 e2) = IntGreaterThanFunc (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr (UintGreaterThanFunc e1 e2) = UintGreaterThanFunc (simplifyExpr e1) (simplifyExpr e2)

simplifyExpr (StringHasPrefixFunc e1 e2) = StringHasPrefixFunc (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr (StringHasSuffixFunc e1 e2) = StringHasSuffixFunc (simplifyExpr e1) (simplifyExpr e2)

simplifyExpr (BytesLessOrEqualFunc e1 e2) = BytesLessOrEqualFunc (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr (DoubleLessOrEqualFunc e1 e2) = DoubleLessOrEqualFunc (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr (IntLessOrEqualFunc e1 e2) = IntLessOrEqualFunc (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr (UintLessOrEqualFunc e1 e2) = UintLessOrEqualFunc (simplifyExpr e1) (simplifyExpr e2)

simplifyExpr (BytesLessThanFunc e1 e2) = BytesLessThanFunc (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr (DoubleLessThanFunc e1 e2) = DoubleLessThanFunc (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr (IntLessThanFunc e1 e2) = IntLessThanFunc (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr (UintLessThanFunc e1 e2) = UintLessThanFunc (simplifyExpr e1) (simplifyExpr e2)

simplifyExpr (BoolNotEqualFunc e1 e2) = BoolNotEqualFunc (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr (DoubleNotEqualFunc e1 e2) = DoubleNotEqualFunc (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr (IntNotEqualFunc e1 e2) = IntNotEqualFunc (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr (UintNotEqualFunc e1 e2) = UintNotEqualFunc (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr (StringNotEqualFunc e1 e2) = StringNotEqualFunc (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr (BytesNotEqualFunc e1 e2) = BytesNotEqualFunc (simplifyExpr e1) (simplifyExpr e2)

simplifyExpr (BytesTypeFunc e) = BytesTypeFunc (simplifyExpr e)
simplifyExpr (BoolTypeFunc e) = BoolTypeFunc (simplifyExpr e)
simplifyExpr (DoubleTypeFunc e) = DoubleTypeFunc (simplifyExpr e)
simplifyExpr (IntTypeFunc e) = IntTypeFunc (simplifyExpr e)
simplifyExpr (UintTypeFunc e) = UintTypeFunc (simplifyExpr e)
simplifyExpr (StringTypeFunc e) = StringTypeFunc (simplifyExpr e)

simplifyExpr (RegexFunc e1 e2) = RegexFunc (simplifyExpr e1) (simplifyExpr e2)

simplifyExpr (DoubleListElemFunc es e) = DoubleListElemFunc (map simplifyExpr es) (simplifyExpr e)

simplifyExpr (IntListElemFunc es e) = IntListElemFunc (map simplifyExpr es) (simplifyExpr e)
simplifyExpr (BytesListLengthFunc es) = Const (length es)
simplifyExpr (BoolListLengthFunc es) = Const (length es)
simplifyExpr (BytesLengthFunc e) = case simplifyExpr e of
        (Const b) -> Const (length b)
        b -> BytesLengthFunc b
simplifyExpr (DoubleListLengthFunc es) = Const (length es)
simplifyExpr (IntListLengthFunc es) = Const (length es)
simplifyExpr (StringListLengthFunc es) = Const (length es)
simplifyExpr (UintListLengthFunc es) = Const (length es)
simplifyExpr (StringLengthFunc e) = case simplifyExpr e of
        (Const b) -> Const (length b)
        b -> StringLengthFunc b

simplifyExpr (UintListElemFunc es e) = UintListElemFunc (map simplifyExpr es) (simplifyExpr e)

simplifyExpr (StringListElemFunc es e) = StringListElemFunc (map simplifyExpr es) (simplifyExpr e)
simplifyExpr (StringToLowerFunc e) = case simplifyExpr e of
        (Const s) -> Const $ map toLower s
        s -> s
simplifyExpr (StringToUpperFunc e) = case simplifyExpr e of
        (Const s) -> Const $ map toUpper s
        s -> s

simplifyExpr (BytesListElemFunc es e) = BytesListElemFunc (map simplifyExpr es) (simplifyExpr e)

simplifyExpr e = e

simplifyOrFunc :: Expr Bool -> Expr Bool -> Expr Bool
simplifyOrFunc true@(Const True) _ = true
simplifyOrFunc _ true@(Const True) = true
simplifyOrFunc (Const False) v = v
simplifyOrFunc v (Const False) = v
simplifyOrFunc v1 v2
    | v1 == v2  = v1
    | v1 == simplifyNotFunc v2 = Const True
    | simplifyNotFunc v1 == v2 = Const True
    | otherwise = OrFunc v1 v2

simplifyAndFunc :: Expr Bool -> Expr Bool -> Expr Bool
simplifyAndFunc (Const True) v = v
simplifyAndFunc v (Const True) = v
simplifyAndFunc false@(Const False) _ = false
simplifyAndFunc _ false@(Const False) = false

simplifyAndFunc v1@(StringEqualFunc s1 s2) (StringEqualFunc s1' s2') = 
    case (s1, s2, s1', s2') of
    (Const c1, StringVariable, Const c2, StringVariable) -> if c1 == c2 then v1 else Const False
    (Const c1, StringVariable, StringVariable, Const c2) -> if c1 == c2 then v1 else Const False
    (StringVariable, Const c1, Const c2, StringVariable) -> if c1 == c2 then v1 else Const False
    (StringVariable, Const c1, StringVariable, Const c2) -> if c1 == c2 then v1 else Const False
simplifyAndFunc v1@(StringEqualFunc s1 s2) (StringNotEqualFunc s1' s2') = 
    case (s1, s2, s1', s2') of
    (Const c1, StringVariable, Const c2, StringVariable) -> if c1 /= c2 then v1 else Const False
    (Const c1, StringVariable, StringVariable, Const c2) -> if c1 /= c2 then v1 else Const False
    (StringVariable, Const c1, Const c2, StringVariable) -> if c1 /= c2 then v1 else Const False
    (StringVariable, Const c1, StringVariable, Const c2) -> if c1 /= c2 then v1 else Const False
simplifyAndFunc v1@(StringNotEqualFunc s1 s2) (StringEqualFunc s1' s2') = 
    case (s1, s2, s1', s2') of
    (Const c1, StringVariable, Const c2, StringVariable) -> if c1 /= c2 then v1 else Const False
    (Const c1, StringVariable, StringVariable, Const c2) -> if c1 /= c2 then v1 else Const False
    (StringVariable, Const c1, Const c2, StringVariable) -> if c1 /= c2 then v1 else Const False
    (StringVariable, Const c1, StringVariable, Const c2) -> if c1 /= c2 then v1 else Const False

simplifyAndFunc v1@(IntEqualFunc s1 s2) (IntEqualFunc s1' s2') = 
    case (s1, s2, s1', s2') of
    (Const c1, IntVariable, Const c2, IntVariable) -> if c1 == c2 then v1 else Const False
    (Const c1, IntVariable, IntVariable, Const c2) -> if c1 == c2 then v1 else Const False
    (IntVariable, Const c1, Const c2, IntVariable) -> if c1 == c2 then v1 else Const False
    (IntVariable, Const c1, IntVariable, Const c2) -> if c1 == c2 then v1 else Const False
simplifyAndFunc v1@(IntEqualFunc s1 s2) (IntNotEqualFunc s1' s2') = 
    case (s1, s2, s1', s2') of
    (Const c1, IntVariable, Const c2, IntVariable) -> if c1 /= c2 then v1 else Const False
    (Const c1, IntVariable, IntVariable, Const c2) -> if c1 /= c2 then v1 else Const False
    (IntVariable, Const c1, Const c2, IntVariable) -> if c1 /= c2 then v1 else Const False
    (IntVariable, Const c1, IntVariable, Const c2) -> if c1 /= c2 then v1 else Const False
simplifyAndFunc v1@(IntNotEqualFunc s1 s2) (IntEqualFunc s1' s2') = 
    case (s1, s2, s1', s2') of
    (Const c1, IntVariable, Const c2, IntVariable) -> if c1 /= c2 then v1 else Const False
    (Const c1, IntVariable, IntVariable, Const c2) -> if c1 /= c2 then v1 else Const False
    (IntVariable, Const c1, Const c2, IntVariable) -> if c1 /= c2 then v1 else Const False
    (IntVariable, Const c1, IntVariable, Const c2) -> if c1 /= c2 then v1 else Const False

simplifyAndFunc v1@(UintEqualFunc s1 s2) (UintEqualFunc s1' s2') = 
    case (s1, s2, s1', s2') of
    (Const c1, UintVariable, Const c2, UintVariable) -> if c1 == c2 then v1 else Const False
    (Const c1, UintVariable, UintVariable, Const c2) -> if c1 == c2 then v1 else Const False
    (UintVariable, Const c1, Const c2, UintVariable) -> if c1 == c2 then v1 else Const False
    (UintVariable, Const c1, UintVariable, Const c2) -> if c1 == c2 then v1 else Const False
simplifyAndFunc v1@(UintEqualFunc s1 s2) (UintNotEqualFunc s1' s2') = 
    case (s1, s2, s1', s2') of
    (Const c1, UintVariable, Const c2, UintVariable) -> if c1 /= c2 then v1 else Const False
    (Const c1, UintVariable, UintVariable, Const c2) -> if c1 /= c2 then v1 else Const False
    (UintVariable, Const c1, Const c2, UintVariable) -> if c1 /= c2 then v1 else Const False
    (UintVariable, Const c1, UintVariable, Const c2) -> if c1 /= c2 then v1 else Const False
simplifyAndFunc v1@(UintNotEqualFunc s1 s2) (UintEqualFunc s1' s2') = 
    case (s1, s2, s1', s2') of
    (Const c1, UintVariable, Const c2, UintVariable) -> if c1 /= c2 then v1 else Const False
    (Const c1, UintVariable, UintVariable, Const c2) -> if c1 /= c2 then v1 else Const False
    (UintVariable, Const c1, Const c2, UintVariable) -> if c1 /= c2 then v1 else Const False
    (UintVariable, Const c1, UintVariable, Const c2) -> if c1 /= c2 then v1 else Const False

simplifyAndFunc v1 v2
    | v1 == v2  = v1
    | v1 == simplifyNotFunc v2 = Const False
    | simplifyNotFunc v1 == v2 = Const False
    | otherwise = AndFunc v1 v2

simplifyNotFunc :: Expr Bool -> Expr Bool
simplifyNotFunc (NotFunc v) = v
simplifyNotFunc (Const True) = Const False
simplifyNotFunc (Const False) = Const True
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