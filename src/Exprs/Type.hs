module Exprs.Type (
    mkTypeExpr
    , typeExpr
) where

import Control.Monad.Except (Except, runExcept, throwError)

import qualified Parsers
import Expr

mkTypeExpr :: [AnyExpr] -> Except String AnyExpr
mkTypeExpr es = do {
    e <- assertArgs1 "type" es; 
    case e of
    (AnyExpr _ (BoolFunc _)) -> mkBoolExpr . typeExpr <$> assertBool e;
    (AnyExpr _ (IntFunc _)) -> mkBoolExpr . typeExpr <$> assertInt e;
    (AnyExpr _ (UintFunc _)) -> mkBoolExpr . typeExpr <$> assertUint e;
    (AnyExpr _ (DoubleFunc _)) -> mkBoolExpr . typeExpr <$> assertDouble e;
    (AnyExpr _ (StringFunc _)) -> mkBoolExpr . typeExpr <$> assertString e;
    (AnyExpr _ (BytesFunc _)) -> mkBoolExpr . typeExpr <$> assertBytes e;
}

typeExpr :: Expr a -> Expr Bool
typeExpr e = Expr {
    desc = mkDesc "type" [desc e]
    , eval = \v -> case runExcept $ eval e v of
        (Left _) -> return False
        (Right _) -> return True
}


