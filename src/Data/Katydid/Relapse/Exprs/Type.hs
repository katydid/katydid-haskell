-- |
-- This module contains the Relapse type expression.

module Data.Katydid.Relapse.Exprs.Type
  ( mkTypeExpr
  , typeExpr
  )
where

import           Data.Katydid.Relapse.Expr

-- |
-- mkTypeExpr is used by the parser to create a type expression for the specific input type.
mkTypeExpr :: [AnyExpr] -> Either String AnyExpr
mkTypeExpr es = do
  e <- assertArgs1 "type" es
  case e of
    (AnyExpr _ (BoolFunc   _)) -> mkBoolExpr . typeExpr <$> assertBool e
    (AnyExpr _ (IntFunc    _)) -> mkBoolExpr . typeExpr <$> assertInt e
    (AnyExpr _ (UintFunc   _)) -> mkBoolExpr . typeExpr <$> assertUint e
    (AnyExpr _ (DoubleFunc _)) -> mkBoolExpr . typeExpr <$> assertDouble e
    (AnyExpr _ (StringFunc _)) -> mkBoolExpr . typeExpr <$> assertString e
    (AnyExpr _ (BytesFunc  _)) -> mkBoolExpr . typeExpr <$> assertBytes e

-- |
-- typeExpr creates an expression that returns true if the containing expression does not return an error.
-- For example: `(typeExpr varBoolExpr)` will ony return true is the field value is a bool.
typeExpr :: Expr a -> Expr Bool
typeExpr e = Expr
  { desc = mkDesc "type" [desc e]
  , eval = \v -> case eval e v of
    (Left  _) -> return False
    (Right _) -> return True
  }


