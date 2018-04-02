module Exprs.Logic (
    mkNotExpr, notExpr
    , mkAndExpr, andExpr
    , mkOrExpr, orExpr
) where

import Control.Monad.Except (Except, runExcept, throwError)

import Expr

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

-- TODO: more simplification rules for AND
-- simplifyAndFunc v1@(StringEqualFunc s1 s2) (StringEqualFunc s1' s2') = 
--     case (s1, s2, s1', s2') of
--     (Const c1, StringVariable, Const c2, StringVariable) -> if c1 == c2 then v1 else Const False
--     (Const c1, StringVariable, StringVariable, Const c2) -> if c1 == c2 then v1 else Const False
--     (StringVariable, Const c1, Const c2, StringVariable) -> if c1 == c2 then v1 else Const False
--     (StringVariable, Const c1, StringVariable, Const c2) -> if c1 == c2 then v1 else Const False
-- simplifyAndFunc v1@(StringEqualFunc s1 s2) (StringNotEqualFunc s1' s2') = 
--     case (s1, s2, s1', s2') of
--     (Const c1, StringVariable, Const c2, StringVariable) -> if c1 /= c2 then v1 else Const False
--     (Const c1, StringVariable, StringVariable, Const c2) -> if c1 /= c2 then v1 else Const False
--     (StringVariable, Const c1, Const c2, StringVariable) -> if c1 /= c2 then v1 else Const False
--     (StringVariable, Const c1, StringVariable, Const c2) -> if c1 /= c2 then v1 else Const False
-- simplifyAndFunc v1@(StringNotEqualFunc s1 s2) (StringEqualFunc s1' s2') = 
--     case (s1, s2, s1', s2') of
--     (Const c1, StringVariable, Const c2, StringVariable) -> if c1 /= c2 then v1 else Const False
--     (Const c1, StringVariable, StringVariable, Const c2) -> if c1 /= c2 then v1 else Const False
--     (StringVariable, Const c1, Const c2, StringVariable) -> if c1 /= c2 then v1 else Const False
--     (StringVariable, Const c1, StringVariable, Const c2) -> if c1 /= c2 then v1 else Const False
-- simplifyAndFunc v1@(IntEqualFunc s1 s2) (IntEqualFunc s1' s2') = 
--     case (s1, s2, s1', s2') of
--     (Const c1, IntVariable, Const c2, IntVariable) -> if c1 == c2 then v1 else Const False
--     (Const c1, IntVariable, IntVariable, Const c2) -> if c1 == c2 then v1 else Const False
--     (IntVariable, Const c1, Const c2, IntVariable) -> if c1 == c2 then v1 else Const False
--     (IntVariable, Const c1, IntVariable, Const c2) -> if c1 == c2 then v1 else Const False
-- simplifyAndFunc v1@(IntEqualFunc s1 s2) (IntNotEqualFunc s1' s2') = 
--     case (s1, s2, s1', s2') of
--     (Const c1, IntVariable, Const c2, IntVariable) -> if c1 /= c2 then v1 else Const False
--     (Const c1, IntVariable, IntVariable, Const c2) -> if c1 /= c2 then v1 else Const False
--     (IntVariable, Const c1, Const c2, IntVariable) -> if c1 /= c2 then v1 else Const False
--     (IntVariable, Const c1, IntVariable, Const c2) -> if c1 /= c2 then v1 else Const False
-- simplifyAndFunc v1@(IntNotEqualFunc s1 s2) (IntEqualFunc s1' s2') = 
--     case (s1, s2, s1', s2') of
--     (Const c1, IntVariable, Const c2, IntVariable) -> if c1 /= c2 then v1 else Const False
--     (Const c1, IntVariable, IntVariable, Const c2) -> if c1 /= c2 then v1 else Const False
--     (IntVariable, Const c1, Const c2, IntVariable) -> if c1 /= c2 then v1 else Const False
--     (IntVariable, Const c1, IntVariable, Const c2) -> if c1 /= c2 then v1 else Const False
-- simplifyAndFunc v1@(UintEqualFunc s1 s2) (UintEqualFunc s1' s2') = 
--     case (s1, s2, s1', s2') of
--     (Const c1, UintVariable, Const c2, UintVariable) -> if c1 == c2 then v1 else Const False
--     (Const c1, UintVariable, UintVariable, Const c2) -> if c1 == c2 then v1 else Const False
--     (UintVariable, Const c1, Const c2, UintVariable) -> if c1 == c2 then v1 else Const False
--     (UintVariable, Const c1, UintVariable, Const c2) -> if c1 == c2 then v1 else Const False
-- simplifyAndFunc v1@(UintEqualFunc s1 s2) (UintNotEqualFunc s1' s2') = 
--     case (s1, s2, s1', s2') of
--     (Const c1, UintVariable, Const c2, UintVariable) -> if c1 /= c2 then v1 else Const False
--     (Const c1, UintVariable, UintVariable, Const c2) -> if c1 /= c2 then v1 else Const False
--     (UintVariable, Const c1, Const c2, UintVariable) -> if c1 /= c2 then v1 else Const False
--     (UintVariable, Const c1, UintVariable, Const c2) -> if c1 /= c2 then v1 else Const False
-- simplifyAndFunc v1@(UintNotEqualFunc s1 s2) (UintEqualFunc s1' s2') = 
--     case (s1, s2, s1', s2') of
--     (Const c1, UintVariable, Const c2, UintVariable) -> if c1 /= c2 then v1 else Const False
--     (Const c1, UintVariable, UintVariable, Const c2) -> if c1 /= c2 then v1 else Const False
--     (UintVariable, Const c1, Const c2, UintVariable) -> if c1 /= c2 then v1 else Const False
--     (UintVariable, Const c1, UintVariable, Const c2) -> if c1 /= c2 then v1 else Const False

-- andExpr' creates an `and` expression, but assumes that both expressions have a var.
andExpr' :: Expr Bool -> Expr Bool -> Expr Bool
andExpr' a b
    | a == b = a
    | name a == "not" && head (params a) == desc b = (boolExpr False)
    | name b == "not" && head (params b) == desc a = (boolExpr False)
    | otherwise = Expr {
        desc = mkDesc "and" [desc a, desc b]
        , eval = \v -> (&&) <$> eval a v <*> eval b v
    }

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