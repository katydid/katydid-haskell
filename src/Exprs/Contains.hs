module Exprs.Contains (
    mkContainsExpr
    , containsStringExpr
    , containsExpr
) where

import Control.Monad.Except (Except, runExcept, throwError)
import Data.List (isInfixOf)

import Expr

mkContainsExpr :: [AnyExpr] -> Except String AnyExpr
mkContainsExpr es = do {
    (e1, e2) <- assertArgs2 "contains" es;
    case e2 of
    (AnyExpr _ (StringFunc _)) -> mkContainsStringExpr' <$> assertString e1 <*> assertString e2
    (AnyExpr _ (StringsFunc _)) -> mkContainsExpr' <$> assertString e1 <*> assertStrings e2
    (AnyExpr _ (IntsFunc _)) -> mkContainsExpr' <$> assertInt e1 <*> assertInts e2
    (AnyExpr _ (UintsFunc _)) -> mkContainsExpr' <$> assertUint e1 <*> assertUints e2
}

mkContainsStringExpr' :: Expr String -> Expr String -> AnyExpr
mkContainsStringExpr' e f = mkBoolExpr $ containsStringExpr e f

containsStringExpr :: Expr String -> Expr String -> Expr Bool
containsStringExpr s sub = trimBool $ Expr {
    desc = mkDesc "contains" [desc s, desc sub]
    , eval = \v -> isInfixOf <$> eval sub v <*> eval s v
}

mkContainsExpr' :: (Eq a) => Expr a -> Expr [a] -> AnyExpr
mkContainsExpr' e f = mkBoolExpr $ containsExpr e f

containsExpr :: (Eq a) => Expr a -> Expr [a] -> Expr Bool
containsExpr e es = trimBool $ Expr {
    desc = mkDesc "contains" [desc e, desc es]
    , eval = \v -> elem <$> eval e v <*> eval es v
}
