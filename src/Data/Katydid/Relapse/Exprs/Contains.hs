-- |
-- This module contains the Relapse contains expressions.
module Data.Katydid.Relapse.Exprs.Contains (
    mkContainsExpr
    , containsStringExpr
    , containsExpr
) where

import qualified Data.Text as Text

import Data.Katydid.Relapse.Expr

-- |
-- mkContainsExpr dynamically creates a contains expression, if the two input types are:
-- 
--     * String and String where the second string is the possible substring.
--     * A List of :Strings, Ints or Uints paired with a String, Int or Uint respectively.
mkContainsExpr :: [AnyExpr] -> Either String AnyExpr
mkContainsExpr es = do {
    (e1, e2) <- assertArgs2 "contains" es;
    case e2 of
    (AnyExpr _ (StringFunc _)) -> mkContainsStringExpr' <$> assertString e1 <*> assertString e2
    (AnyExpr _ (StringsFunc _)) -> mkContainsExpr' <$> assertString e1 <*> assertStrings e2
    (AnyExpr _ (IntsFunc _)) -> mkContainsExpr' <$> assertInt e1 <*> assertInts e2
    (AnyExpr _ (UintsFunc _)) -> mkContainsExpr' <$> assertUint e1 <*> assertUints e2
}

mkContainsStringExpr' :: Expr Text.Text -> Expr Text.Text -> AnyExpr
mkContainsStringExpr' e f = mkBoolExpr $ containsStringExpr e f

-- |
-- containsStringExpr creates a contains expression that returns true if the second string is a substring of the first.
containsStringExpr :: Expr Text.Text -> Expr Text.Text -> Expr Bool
containsStringExpr s sub = trimBool Expr {
    desc = mkDesc "contains" [desc s, desc sub]
    , eval = \v -> Text.isInfixOf <$> eval sub v <*> eval s v
}

mkContainsExpr' :: (Eq a) => Expr a -> Expr [a] -> AnyExpr
mkContainsExpr' e f = mkBoolExpr $ containsExpr e f

-- |
-- containsExpr creates a contains expression that returns true if the first argument is an element in the second list argument.
containsExpr :: (Eq a) => Expr a -> Expr [a] -> Expr Bool
containsExpr e es = trimBool Expr {
    desc = mkDesc "contains" [desc e, desc es]
    , eval = \v -> elem <$> eval e v <*> eval es v
}
