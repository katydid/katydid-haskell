module Exprs.Strings (
    mkHasPrefixExpr, hasPrefixExpr
    , mkHasSuffixExpr, hasSuffixExpr
    , mkRegexExpr, regexExpr
    , mkToLowerExpr, toLowerExpr
    , mkToUpperExpr, toUpperExpr
) where

import Control.Monad.Except (Except, runExcept, throwError)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Char (toLower, toUpper)
import Text.Regex.TDFA ((=~))

import Expr

mkHasPrefixExpr :: [AnyExpr] -> Except String AnyExpr
mkHasPrefixExpr es = do {
    (e1, e2) <- assertArgs2 "hasPrefix" es;
    s1 <- assertString e1;
    s2 <- assertString e2;
    return $ mkBoolExpr $ hasPrefixExpr s1 s2;
}

hasPrefixExpr :: Expr String -> Expr String -> Expr Bool
hasPrefixExpr e1 e2 = trimBool $ Expr {
    desc = mkDesc "hasPrefix" [desc e1, desc e2]
    , eval = \v -> isPrefixOf <$> eval e2 v <*> eval e1 v
}

mkHasSuffixExpr :: [AnyExpr] -> Except String AnyExpr
mkHasSuffixExpr es = do {
    (e1, e2) <- assertArgs2 "hasSuffix" es;
    s1 <- assertString e1;
    s2 <- assertString e2;
    return $ mkBoolExpr $ hasSuffixExpr s1 s2;
}

hasSuffixExpr :: Expr String -> Expr String -> Expr Bool
hasSuffixExpr e1 e2 = trimBool $ Expr {
    desc = mkDesc "hasSuffix" [desc e1, desc e2]
    , eval = \v -> isSuffixOf <$> eval e2 v <*> eval e1 v
}

mkRegexExpr :: [AnyExpr] -> Except String AnyExpr
mkRegexExpr es = do {
    (e1, e2) <- assertArgs2 "regex" es;
    e <- assertString e1;
    s <- assertString e2;
    return $ mkBoolExpr $ regexExpr e s;
}

regexExpr :: Expr String -> Expr String -> Expr Bool
regexExpr e s = trimBool $ Expr {
    desc = mkDesc "regex" [desc e, desc s]
    , eval = \v -> (=~) <$> eval s v <*> eval e v
}

mkToLowerExpr :: [AnyExpr] -> Except String AnyExpr
mkToLowerExpr es = do {
    e <- assertArgs1 "toLower" es;
    s <- assertString e;
    return $ mkStringExpr $ toLowerExpr s;
}

toLowerExpr :: Expr String -> Expr String
toLowerExpr e = trimString $ Expr {
    desc = mkDesc "toLower" [desc e]
    , eval = \v -> map toLower <$> eval e v
}

mkToUpperExpr :: [AnyExpr] -> Except String AnyExpr
mkToUpperExpr es = do {
    e <- assertArgs1 "toUpper" es;
    s <- assertString e;
    return $ mkStringExpr $ toUpperExpr s;
}

toUpperExpr :: Expr String -> Expr String
toUpperExpr e = trimString $ Expr {
    desc = mkDesc "toUpper" [desc e]
    , eval = \v -> map toUpper <$> eval e v
}
