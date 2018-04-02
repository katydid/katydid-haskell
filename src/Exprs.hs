module Exprs (
    mkBuiltIn
    , mkExpr
    , MkFunc
    , stdOnly
) where

import Control.Monad.Except (Except, throwError)

import Expr
import Exprs.Compare
import Exprs.Contains
import Exprs.Elem
import Exprs.Length
import Exprs.Logic
import Exprs.Strings
import Exprs.Type
import Exprs.Var

type MkFunc = String -> [AnyExpr] -> Except String AnyExpr

mkExpr :: String -> [AnyExpr] -> Except String AnyExpr
mkExpr "eq" es = mkEqExpr es
mkExpr "ne" es = mkNeExpr es
mkExpr "ge" es = mkGeExpr es
mkExpr "gt" es = mkGtExpr es
mkExpr "le" es = mkLeExpr es
mkExpr "lt" es = mkLtExpr es
mkExpr "contains" es = mkContainsExpr es
mkExpr "elem" es = mkElemExpr es
mkExpr "length" es = mkLengthExpr es
mkExpr "not" es = mkNotExpr es
mkExpr "and" es = mkAndExpr es
mkExpr "or" es = mkOrExpr es
mkExpr "hasPrefix" es = mkHasPrefixExpr es
mkExpr "hasSuffix" es = mkHasSuffixExpr es
mkExpr "regex" es = mkRegexExpr es
mkExpr "toLower" es = mkToLowerExpr es
mkExpr "toUpper" es = mkToUpperExpr es
mkExpr "type" es = mkTypeExpr es
mkExpr n _ = throwError $ "unknown function: " ++ n

stdOnly :: String -> [AnyExpr] -> Except String AnyExpr
stdOnly n _ = throwError $ "unknown function: " ++ n

-- |
-- mkBuiltIn parsers a builtin function to a relapse expression.
mkBuiltIn :: String -> AnyExpr -> Except String AnyExpr
mkBuiltIn symbol constExpr = funcName symbol >>= (\n ->
        if n == "type" then
            mkExpr n [constExpr]
        else if n == "regex" then
            mkExpr n [constExpr, constToVar constExpr]
        else
            mkExpr n [constToVar constExpr, constExpr]
    )

funcName :: String -> Except String String
funcName "==" = return "eq"
funcName "!=" = return "ne"
funcName "<" = return "lt"
funcName ">" = return "gt"
funcName "<=" = return "le"
funcName ">=" = return "ge"
funcName "~=" = return "regex"
funcName "*=" = return "contains"
funcName "^=" = return "hasPrefix"
funcName "$=" = return "hasSuffix"
funcName "::" = return "type"
funcName n = fail $ "unexpected funcName: <" ++ n ++ ">"

constToVar :: AnyExpr -> AnyExpr
constToVar (AnyExpr _ (BoolFunc _)) = mkBoolExpr varBoolExpr
constToVar (AnyExpr _ (IntFunc _)) = mkIntExpr varIntExpr
constToVar (AnyExpr _ (UintFunc _)) = mkUintExpr varUintExpr
constToVar (AnyExpr _ (DoubleFunc _)) = mkDoubleExpr varDoubleExpr
constToVar (AnyExpr _ (StringFunc _)) = mkStringExpr varStringExpr
constToVar (AnyExpr _ (BytesFunc _)) = mkBytesExpr varBytesExpr
