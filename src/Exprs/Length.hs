module Exprs.Length (
    mkLengthExpr
    , lengthListExpr
    , lengthStringExpr
    , lengthBytesExpr
) where

import Control.Monad.Except (Except, runExcept, throwError)
import qualified Data.Text as Text
import qualified Data.ByteString as ByteString

import Expr

mkLengthExpr :: [AnyExpr] -> Except String AnyExpr
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

lengthListExpr :: Expr [a] -> Expr Int
lengthListExpr e = trimInt Expr {
    desc = mkDesc "length" [desc e]
    , eval = \v -> length <$> eval e v
}

lengthStringExpr :: Expr Text.Text -> Expr Int
lengthStringExpr e = trimInt Expr {
    desc = mkDesc "length" [desc e]
    , eval = \v -> Text.length <$> eval e v
}

lengthBytesExpr :: Expr ByteString.ByteString -> Expr Int
lengthBytesExpr e = trimInt Expr {
    desc = mkDesc "length" [desc e]
    , eval = \v -> ByteString.length <$> eval e v
}

