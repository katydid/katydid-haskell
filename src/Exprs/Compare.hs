module Exprs.Compare (
    mkEqExpr, eqExpr
    , mkNeExpr, neExpr
    , mkGeExpr, geExpr
    , mkLeExpr, leExpr
    , mkGtExpr, gtExpr
    , mkLtExpr, ltExpr
) where

import Control.Monad.Except (Except, runExcept, throwError)

import Expr

mkEqExpr :: [AnyExpr] -> Except String AnyExpr
mkEqExpr es = do {
    (e1, e2) <- assertArgs2 "eq" es;
    case e1 of
    (AnyExpr _ (BoolFunc _)) -> mkEqExpr' <$> assertBool e1 <*> assertBool e2
    (AnyExpr _ (IntFunc _)) -> mkEqExpr' <$> assertInt e1 <*> assertInt e2
    (AnyExpr _ (UintFunc _)) ->  mkEqExpr' <$> assertUint e1 <*> assertUint e2
    (AnyExpr _ (DoubleFunc _)) -> mkEqExpr' <$> assertDouble e1 <*> assertDouble e2
    (AnyExpr _ (StringFunc _)) -> mkEqExpr' <$> assertString e1 <*> assertString e2
    (AnyExpr _ (BytesFunc _)) -> mkEqExpr' <$> assertBytes e1 <*> assertBytes e2
}

mkEqExpr' :: (Eq a) => Expr a -> Expr a -> AnyExpr
mkEqExpr' e f = mkBoolExpr $ eqExpr e f

eqExpr :: (Eq a) => Expr a -> Expr a -> Expr Bool
eqExpr a b = trimBool Expr {
    desc = mkDesc "eq" [desc a, desc b]
    , eval = \v -> eq (runExcept $ eval a v) (runExcept $ eval b v)
}

eq :: (Eq a) => Either String a -> Either String a -> Except String Bool
eq (Right v1) (Right v2) = return $ v1 == v2
eq (Left _) _ = return False
eq _ (Left _) = return False

mkNeExpr :: [AnyExpr] -> Except String AnyExpr
mkNeExpr es = do {
    (e1, e2) <- assertArgs2 "ne" es;
    case e1 of
    (AnyExpr _ (BoolFunc _)) -> mkNeExpr' <$> assertBool e1 <*> assertBool e2
    (AnyExpr _ (IntFunc _)) -> mkNeExpr' <$> assertInt e1 <*> assertInt e2
    (AnyExpr _ (UintFunc _)) ->  mkNeExpr' <$> assertUint e1 <*> assertUint e2
    (AnyExpr _ (DoubleFunc _)) -> mkNeExpr' <$> assertDouble e1 <*> assertDouble e2
    (AnyExpr _ (StringFunc _)) -> mkNeExpr' <$> assertString e1 <*> assertString e2
    (AnyExpr _ (BytesFunc _)) -> mkNeExpr' <$> assertBytes e1 <*> assertBytes e2
}

mkNeExpr' :: (Eq a) => Expr a -> Expr a -> AnyExpr
mkNeExpr' e f = mkBoolExpr $ neExpr e f

neExpr :: (Eq a) => Expr a -> Expr a -> Expr Bool
neExpr a b = trimBool Expr {
    desc = mkDesc "ne" [desc a, desc b]
    , eval = \v -> ne (runExcept $ eval a v) (runExcept $ eval b v)
}

ne :: (Eq a) => Either String a -> Either String a -> Except String Bool
ne (Right v1) (Right v2) = return $ v1 /= v2
ne (Left _) _ = return False
ne _ (Left _) = return False

mkGeExpr :: [AnyExpr] -> Except String AnyExpr
mkGeExpr es = do {
    (e1, e2) <- assertArgs2 "ge" es;
    case e1 of
    (AnyExpr _ (IntFunc _)) -> mkGeExpr' <$> assertInt e1 <*> assertInt e2
    (AnyExpr _ (UintFunc _)) ->  mkGeExpr' <$> assertUint e1 <*> assertUint e2
    (AnyExpr _ (DoubleFunc _)) -> mkGeExpr' <$> assertDouble e1 <*> assertDouble e2
    (AnyExpr _ (BytesFunc _)) -> mkGeExpr' <$> assertBytes e1 <*> assertBytes e2
}

mkGeExpr' :: (Ord a) => Expr a -> Expr a -> AnyExpr
mkGeExpr' e f = mkBoolExpr $ geExpr e f

geExpr :: (Ord a) => Expr a -> Expr a -> Expr Bool
geExpr a b = trimBool Expr {
    desc = mkDesc "ge" [desc a, desc b]
    , eval = \v -> ge (runExcept $ eval a v) (runExcept $ eval b v)
}

ge :: (Ord a) => Either String a -> Either String a -> Except String Bool
ge (Right v1) (Right v2) = return $ v1 >= v2
ge (Left _) _ = return False
ge _ (Left _) = return False

mkGtExpr :: [AnyExpr] -> Except String AnyExpr
mkGtExpr es = do {
    (e1, e2) <- assertArgs2 "gt" es;
    case e1 of
    (AnyExpr _ (IntFunc _)) -> mkGtExpr' <$> assertInt e1 <*> assertInt e2
    (AnyExpr _ (UintFunc _)) ->  mkGtExpr' <$> assertUint e1 <*> assertUint e2
    (AnyExpr _ (DoubleFunc _)) -> mkGtExpr' <$> assertDouble e1 <*> assertDouble e2
    (AnyExpr _ (BytesFunc _)) -> mkGtExpr' <$> assertBytes e1 <*> assertBytes e2
}

mkGtExpr' :: (Ord a) => Expr a -> Expr a -> AnyExpr
mkGtExpr' e f = mkBoolExpr $ gtExpr e f

gtExpr :: (Ord a) => Expr a -> Expr a -> Expr Bool
gtExpr a b = trimBool Expr {
    desc = mkDesc "gt" [desc a, desc b]
    , eval = \v -> gt (runExcept $ eval a v) (runExcept $ eval b v)
}

gt :: (Ord a) => Either String a -> Either String a -> Except String Bool
gt (Right v1) (Right v2) = return $ v1 > v2
gt (Left _) _ = return False
gt _ (Left _) = return False

mkLeExpr :: [AnyExpr] -> Except String AnyExpr
mkLeExpr es = do {
    (e1, e2) <- assertArgs2 "le" es;
    case e1 of
    (AnyExpr _ (IntFunc _)) -> mkLeExpr' <$> assertInt e1 <*> assertInt e2
    (AnyExpr _ (UintFunc _)) ->  mkLeExpr' <$> assertUint e1 <*> assertUint e2
    (AnyExpr _ (DoubleFunc _)) -> mkLeExpr' <$> assertDouble e1 <*> assertDouble e2
    (AnyExpr _ (BytesFunc _)) -> mkLeExpr' <$> assertBytes e1 <*> assertBytes e2
}

mkLeExpr' :: (Ord a) => Expr a -> Expr a -> AnyExpr
mkLeExpr' e f = mkBoolExpr $ leExpr e f

leExpr :: (Ord a) => Expr a -> Expr a -> Expr Bool
leExpr a b = trimBool Expr {
    desc = mkDesc "le" [desc a, desc b]
    , eval = \v -> le (runExcept $ eval a v) (runExcept $ eval b v)
}

le :: (Ord a) => Either String a -> Either String a -> Except String Bool
le (Right v1) (Right v2) = return $ v1 <= v2
le (Left _) _ = return False
le _ (Left _) = return False

mkLtExpr :: [AnyExpr] -> Except String AnyExpr
mkLtExpr es = do {
    (e1, e2) <- assertArgs2 "lt" es;
    case e1 of
    (AnyExpr _ (IntFunc _)) -> mkLtExpr' <$> assertInt e1 <*> assertInt e2
    (AnyExpr _ (UintFunc _)) ->  mkLtExpr' <$> assertUint e1 <*> assertUint e2
    (AnyExpr _ (DoubleFunc _)) -> mkLtExpr' <$> assertDouble e1 <*> assertDouble e2
    (AnyExpr _ (BytesFunc _)) -> mkLtExpr' <$> assertBytes e1 <*> assertBytes e2
}

mkLtExpr' :: (Ord a) => Expr a -> Expr a -> AnyExpr
mkLtExpr' e f = mkBoolExpr $ ltExpr e f

ltExpr :: (Ord a) => Expr a -> Expr a -> Expr Bool
ltExpr a b = trimBool Expr {
    desc = mkDesc "lt" [desc a, desc b]
    , eval = \v -> lt (runExcept $ eval a v) (runExcept $ eval b v)
}

lt :: (Ord a) => Either String a -> Either String a -> Except String Bool
lt (Right v1) (Right v2) = return $ v1 < v2
lt (Left _) _ = return False
lt _ (Left _) = return False
