-- |
-- This module contains the Relapse length expressions.
module Data.Katydid.Relapse.Exprs.Length (
    mkLengthExpr
    , lengthListExpr
    , lengthStringExpr
    , lengthBytesExpr
) where

import qualified Data.Text as Text
import qualified Data.ByteString as ByteString

import Data.Katydid.Relapse.Expr

-- |
-- mkLengthExpr dynamically creates a length expression, if the single argument is a list, string or bytes.
mkLengthExpr :: [AnyExpr] -> Either String AnyExpr
mkLengthExpr es = do {
    e <- assertArgs1 "length" es;
    case e of
    (AnyExpr _ (BoolsFunc _)) -> mkIntExpr . lengthListExpr <$> assertBools e;
    (AnyExpr _ (IntsFunc _)) -> mkIntExpr . lengthListExpr <$> assertInts e;
    (AnyExpr _ (UintsFunc _)) -> mkIntExpr . lengthListExpr <$> assertUints e;
    (AnyExpr _ (DoublesFunc _)) -> mkIntExpr . lengthListExpr <$> assertDoubles e;
    (AnyExpr _ (StringsFunc _)) -> mkIntExpr . lengthListExpr <$> assertStrings e;
    (AnyExpr _ (ListOfBytesFunc _)) -> mkIntExpr . lengthListExpr <$> assertListOfBytes e;
    (AnyExpr _ (StringFunc _)) -> mkIntExpr . lengthStringExpr <$> assertString e;
    (AnyExpr _ (BytesFunc _)) -> mkIntExpr . lengthBytesExpr <$> assertBytes e;
}

-- |
-- lengthListExpr creates a length expression, that returns the length of a list.
lengthListExpr :: Expr [a] -> Expr Int
lengthListExpr e = trimInt Expr {
    desc = mkDesc "length" [desc e]
    , eval = \v -> length <$> eval e v
}

-- |
-- lengthStringExpr creates a length expression, that returns the length of a string.
lengthStringExpr :: Expr Text.Text -> Expr Int
lengthStringExpr e = trimInt Expr {
    desc = mkDesc "length" [desc e]
    , eval = \v -> Text.length <$> eval e v
}

-- |
-- lengthBytesExpr creates a length expression, that returns the length of bytes.
lengthBytesExpr :: Expr ByteString.ByteString -> Expr Int
lengthBytesExpr e = trimInt Expr {
    desc = mkDesc "length" [desc e]
    , eval = \v -> ByteString.length <$> eval e v
}

