module Exprs.Logic (
    mkNotExpr, notExpr
    , mkAndExpr, andExpr
    , mkOrExpr, orExpr
) where

import Control.Monad.Except (Except, runExcept, throwError)

import Expr
import Exprs.Var

mkNotExpr :: [AnyExpr] -> Except String AnyExpr
mkNotExpr es = do {
    e <- assertArgs1 "not" es;
    b <- assertBool e;
    return $ mkBoolExpr (notExpr b);
}

notExpr :: Expr Bool -> Expr Bool
notExpr e = if hasVar e 
    then Expr {
            desc = notDesc (desc e)
            , eval = \v -> not <$> eval e v
        }
    else trimBool $ Expr (mkDesc "not" [desc e]) (\v -> not <$> eval e v) 

-- notDesc superficially pushes not operators down to normalize functions.
-- Normalizing functions increases the chances of finding equal expressions and being able to simplify patterns.
notDesc :: Desc -> Desc
notDesc d
    | _name d == "not" = 
        let child0 = head $ _params d
        in mkDesc (_name child0) (_params child0)
    | _name d == "and" =
        let (left:right:[]) = _params d
        in mkDesc "or" [mkDesc "not" [left], mkDesc "not" [right]]
    | _name d == "or" =
        let (left:right:[]) = _params d
        in mkDesc "and" [mkDesc "not" [left], mkDesc "not" [right]]
    | _name d == "ne" = mkDesc "eq" $  _params d
    | _name d == "eq" = mkDesc "ne" $ _params d
    | otherwise = mkDesc "not" [d]

mkAndExpr :: [AnyExpr] -> Except String AnyExpr
mkAndExpr es = do {
    (e1, e2) <- assertArgs2 "and" es;
    b1 <- assertBool e1;
    b2 <- assertBool e2;
    return $ mkBoolExpr $ andExpr b1 b2;
}

andExpr :: Expr Bool -> Expr Bool -> Expr Bool
andExpr a b = case (evalConst a, evalConst b) of
    (Just False, _) -> (boolExpr False)
    (_, Just False) -> (boolExpr False)
    (Just True, _) -> b
    (_, Just True) -> a
    _ -> andExpr' a b

-- andExpr' creates an `and` expression, but assumes that both expressions have a var.
andExpr' :: Expr Bool -> Expr Bool -> Expr Bool
andExpr' a b
    | a == b = a
    | name a == "not" && head (params a) == desc b = (boolExpr False)
    | name b == "not" && head (params b) == desc a = (boolExpr False)
    | name a == "eq" && name b == "eq" = case (varAndConst a, varAndConst b) of
        (Just ca, Just cb) -> if ca == cb then a else boolExpr False
        _ -> defaultAnd a b
    | name a == "eq" && name b == "ne" = case (varAndConst a, varAndConst b) of
        (Just ca, Just cb) -> if ca == cb then boolExpr False else a
        _ -> defaultAnd a b
    | name a == "ne" && name b == "eq" = case (varAndConst a, varAndConst b) of
        (Just ca, Just cb) -> if ca == cb then boolExpr False else b
        _ -> defaultAnd a b
    | otherwise = defaultAnd a b

defaultAnd :: Expr Bool -> Expr Bool -> Expr Bool
defaultAnd a b = Expr {
    desc = mkDesc "and" [desc a, desc b]
    , eval = \v -> (&&) <$> eval a v <*> eval b v
}

varAndConst :: Expr Bool -> Maybe Desc
varAndConst e = let ps = params e
    in if length ps /= 2 then Nothing
    else let (a:b:[]) = ps in
        if isVar a && isConst b then Just b
        else if isVar b && isConst a then Just a
        else Nothing

mkOrExpr :: [AnyExpr] -> Except String AnyExpr
mkOrExpr es = do {
    (e1, e2) <- assertArgs2 "or" es;
    b1 <- assertBool e1;
    b2 <- assertBool e2;
    return $ mkBoolExpr $ orExpr b1 b2;
}

orExpr :: Expr Bool -> Expr Bool -> Expr Bool
orExpr a b = case (evalConst a, evalConst b) of
    (Just True, _) -> (boolExpr True)
    (_, Just True) -> (boolExpr True)
    (Just False, _) -> b
    (_, Just False) -> a
    _ -> orExpr' a b

-- orExpr' creates an `or` expression, but assumes that both expressions have a var.
orExpr' :: Expr Bool -> Expr Bool -> Expr Bool
orExpr' a b
    | a == b = a
    | name a == "not" && head (params a) == desc b = (boolExpr True)
    | name b == "not" && head (params b) == desc a = (boolExpr True)
    | otherwise = Expr {
        desc = mkDesc "or" [desc a, desc b]
        , eval = \v -> (||) <$> eval a v <*> eval b v
    }