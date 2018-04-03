module Exprs.Strings (
    mkHasPrefixExpr, hasPrefixExpr
    , mkHasSuffixExpr, hasSuffixExpr
    , mkRegexExpr, regexExpr
    , mkToLowerExpr, toLowerExpr
    , mkToUpperExpr, toUpperExpr
) where

import Control.Monad.Except (Except, runExcept, throwError)
import Text.Regex.TDFA ((=~))
import Data.Text (Text, isPrefixOf, isSuffixOf, toLower, toUpper, unpack)

import Expr

mkHasPrefixExpr :: [AnyExpr] -> Except String AnyExpr
mkHasPrefixExpr es = do {
    (e1, e2) <- assertArgs2 "hasPrefix" es;
    s1 <- assertString e1;
    s2 <- assertString e2;
    return $ mkBoolExpr $ hasPrefixExpr s1 s2;
}

hasPrefixExpr :: Expr Text -> Expr Text -> Expr Bool
hasPrefixExpr e1 e2 = trimBool Expr {
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

hasSuffixExpr :: Expr Text -> Expr Text -> Expr Bool
hasSuffixExpr e1 e2 = trimBool Expr {
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

regexExpr :: Expr Text -> Expr Text -> Expr Bool
regexExpr e s = trimBool Expr {
    desc = mkDesc "regex" [desc e, desc s]
    , eval = \v -> do {
        s1 <- eval s v;
        e1 <- eval e v;
        return $ (=~) (unpack s1) (unpack e1);
    }
}

mkToLowerExpr :: [AnyExpr] -> Except String AnyExpr
mkToLowerExpr es = do {
    e <- assertArgs1 "toLower" es;
    s <- assertString e;
    return $ mkStringExpr $ toLowerExpr s;
}

toLowerExpr :: Expr Text -> Expr Text
toLowerExpr e = trimString Expr {
    desc = mkDesc "toLower" [desc e]
    , eval = \v -> toLower <$> eval e v
}

mkToUpperExpr :: [AnyExpr] -> Except String AnyExpr
mkToUpperExpr es = do {
    e <- assertArgs1 "toUpper" es;
    s <- assertString e;
    return $ mkStringExpr $ toUpperExpr s;
}

toUpperExpr :: Expr Text -> Expr Text
toUpperExpr e = trimString Expr {
    desc = mkDesc "toUpper" [desc e]
    , eval = \v -> toUpper <$> eval e v
}
