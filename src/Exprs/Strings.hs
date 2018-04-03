-- |
-- This module contains the Relapse string expressions.

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

-- |
-- mkHasPrefixExpr dynamically creates a hasPrefix expression.
mkHasPrefixExpr :: [AnyExpr] -> Except String AnyExpr
mkHasPrefixExpr es = do {
    (e1, e2) <- assertArgs2 "hasPrefix" es;
    s1 <- assertString e1;
    s2 <- assertString e2;
    return $ mkBoolExpr $ hasPrefixExpr s1 s2;
}

-- |
-- hasPrefixExpr creates a hasPrefix expression that returns true if the second is a prefix of the first.
hasPrefixExpr :: Expr Text -> Expr Text -> Expr Bool
hasPrefixExpr e1 e2 = trimBool Expr {
    desc = mkDesc "hasPrefix" [desc e1, desc e2]
    , eval = \v -> isPrefixOf <$> eval e2 v <*> eval e1 v
}

-- |
-- mkHasSuffixExpr dynamically creates a hasSuffix expression.
mkHasSuffixExpr :: [AnyExpr] -> Except String AnyExpr
mkHasSuffixExpr es = do {
    (e1, e2) <- assertArgs2 "hasSuffix" es;
    s1 <- assertString e1;
    s2 <- assertString e2;
    return $ mkBoolExpr $ hasSuffixExpr s1 s2;
}

-- |
-- hasSuffixExpr creates a hasSuffix expression that returns true if the second is a suffix of the first.
hasSuffixExpr :: Expr Text -> Expr Text -> Expr Bool
hasSuffixExpr e1 e2 = trimBool Expr {
    desc = mkDesc "hasSuffix" [desc e1, desc e2]
    , eval = \v -> isSuffixOf <$> eval e2 v <*> eval e1 v
}

-- |
-- mkRegexExpr dynamically creates a regex expression.
mkRegexExpr :: [AnyExpr] -> Except String AnyExpr
mkRegexExpr es = do {
    (e1, e2) <- assertArgs2 "regex" es;
    e <- assertString e1;
    s <- assertString e2;
    return $ mkBoolExpr $ regexExpr e s;
}

-- |
-- regexExpr creates a regex expression that returns true if the first expression matches the second string. 
regexExpr :: Expr Text -> Expr Text -> Expr Bool
regexExpr e s = trimBool Expr {
    desc = mkDesc "regex" [desc e, desc s]
    , eval = \v -> do {
        s1 <- eval s v;
        e1 <- eval e v;
        return $ (=~) (unpack s1) (unpack e1);
    }
}

-- |
-- mkToLowerExpr dynamically creates a toLower expression.
mkToLowerExpr :: [AnyExpr] -> Except String AnyExpr
mkToLowerExpr es = do {
    e <- assertArgs1 "toLower" es;
    s <- assertString e;
    return $ mkStringExpr $ toLowerExpr s;
}

-- |
-- toLowerExpr creates a toLower expression that converts the input string to a lowercase string.
toLowerExpr :: Expr Text -> Expr Text
toLowerExpr e = trimString Expr {
    desc = mkDesc "toLower" [desc e]
    , eval = \v -> toLower <$> eval e v
}

-- |
-- mkToUpperExpr dynamically creates a toUpper expression.
mkToUpperExpr :: [AnyExpr] -> Except String AnyExpr
mkToUpperExpr es = do {
    e <- assertArgs1 "toUpper" es;
    s <- assertString e;
    return $ mkStringExpr $ toUpperExpr s;
}

-- |
-- toUpperExpr creates a toUpper expression that converts the input string to an uppercase string.
toUpperExpr :: Expr Text -> Expr Text
toUpperExpr e = trimString Expr {
    desc = mkDesc "toUpper" [desc e]
    , eval = \v -> toUpper <$> eval e v
}
