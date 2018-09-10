-- |
-- This module contains the standard library of expressions, used by the Relapse parser.

module Data.Katydid.Relapse.Exprs (
    mkBuiltIn
    , mkExpr
    , MkFunc
    , stdOnly
) where

import Data.Katydid.Relapse.Expr
import Data.Katydid.Relapse.Exprs.Compare
import Data.Katydid.Relapse.Exprs.Contains
import Data.Katydid.Relapse.Exprs.Elem
import Data.Katydid.Relapse.Exprs.Length
import Data.Katydid.Relapse.Exprs.Logic
import Data.Katydid.Relapse.Exprs.Strings
import Data.Katydid.Relapse.Exprs.Type
import Data.Katydid.Relapse.Exprs.Var

-- |
-- MkFunc is used by the parser to create a function from a name and arguments.
type MkFunc = String -> [AnyExpr] -> Either String AnyExpr

-- |
-- mkExpr is a grouping of all the standard library functions as one MkFunc.
mkExpr :: String -> [AnyExpr] -> Either String AnyExpr
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
mkExpr n _ = Left $ "unknown function: " ++ n

-- |
-- stdOnly contains no functions, which means that when it is combined 
-- (in Relapse parser) with mkExpr the parser will have access to only the standard library.
stdOnly :: String -> [AnyExpr] -> Either String AnyExpr
stdOnly n _ = Left $ "unknown function: " ++ n

-- |
-- mkBuiltIn parsers a builtin function to a relapse expression.
mkBuiltIn :: String -> AnyExpr -> Either String AnyExpr
mkBuiltIn symbol constExpr = funcName symbol >>= (\n ->
        if n == "type" then
            mkExpr n [constExpr]
        else if n == "regex" then
            mkExpr n [constExpr, constToVar constExpr]
        else
            mkExpr n [constToVar constExpr, constExpr]
    )

funcName :: String -> Either String String
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
