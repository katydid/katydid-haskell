-- |
-- This module contains the Relapse elem expression.
module Data.Katydid.Relapse.Exprs.Elem (
    mkElemExpr
    , elemExpr
) where

import Data.Katydid.Relapse.Expr

-- |
-- mkElemExpr dynamically creates an elem expression, if the first argument is a list and the second an int index.
mkElemExpr :: [AnyExpr] -> Either String AnyExpr
mkElemExpr es = do {
    (e1, e2) <- assertArgs2 "elem" es;
    case e1 of
    (AnyExpr _ (BoolsFunc _)) -> mkElemExpr' mkBoolExpr <$> assertBools e1 <*> assertInt e2
    (AnyExpr _ (IntsFunc _)) -> mkElemExpr' mkIntExpr <$> assertInts e1 <*> assertInt e2
    (AnyExpr _ (UintsFunc _)) -> mkElemExpr' mkUintExpr <$> assertUints e1 <*> assertInt e2
    (AnyExpr _ (DoublesFunc _)) -> mkElemExpr' mkDoubleExpr <$> assertDoubles e1 <*> assertInt e2
    (AnyExpr _ (StringsFunc _)) -> mkElemExpr' mkStringExpr <$> assertStrings e1 <*> assertInt e2
    (AnyExpr _ (ListOfBytesFunc _)) -> mkElemExpr' mkBytesExpr <$> assertListOfBytes e1 <*> assertInt e2
}

mkElemExpr' :: (Expr a -> AnyExpr) -> Expr [a] -> Expr Int -> AnyExpr
mkElemExpr' mk list index =  mk $ elemExpr list index

-- | 
-- elemExpr creates an expression that returns an element from the list at the specified index.
-- Trimming this function would cause it to become non generic.
-- It is not necessary to trim each function, since it is just an optimization.
elemExpr :: Expr [a] -> Expr Int -> Expr a
elemExpr a b = Expr {
    desc = mkDesc "elem" [desc a, desc b]
    , eval = \v -> (!!) <$> eval a v <*> eval b v
}
