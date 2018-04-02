module Exprs.Var (
    varBoolExpr
    , varIntExpr
    , varUintExpr
    , varDoubleExpr
    , varStringExpr
    , varBytesExpr
) where

import Control.Monad.Except (Except, runExcept, throwError)

import qualified Parsers
import Expr

varBoolExpr :: Expr Bool
varBoolExpr = Expr {
    desc = Desc {
        _name = "$bool"
        , _toStr = "$bool"
        , _hash = hashWithName "$bool" []
        , _params = []
        , _hasVar = True
    }
    , eval = \l -> case l of
        (Parsers.Bool b) -> return b
        _ -> throwError "not a bool"
}

varIntExpr :: Expr Int
varIntExpr = Expr {
    desc = Desc {
        _name = "$int"
        , _toStr = "$int"
        , _hash = hashWithName "$int" []
        , _params = []
        , _hasVar = True
    }
    , eval = \l -> case l of
        (Parsers.Number r) -> return $ truncate r
        _ -> throwError "not an int"
}

varUintExpr :: Expr Uint
varUintExpr = Expr {
    desc = Desc {
        _name = "$uint"
        , _toStr = "$uint"
        , _hash = hashWithName "$uint" []
        , _params = []
        , _hasVar = True
    }
    , eval = \l -> case l of
        (Parsers.Number r) -> return $ truncate r
        _ -> throwError "not a uint"
}

varDoubleExpr :: Expr Double
varDoubleExpr = Expr {
    desc = Desc {
        _name = "$double"
        , _toStr = "$double"
        , _hash = hashWithName "$double" []
        , _params = []
        , _hasVar = True
    }
    , eval = \l -> case l of
        (Parsers.Number r) -> return $ fromRational r
        _ -> throwError "not a double"
}

varStringExpr :: Expr String
varStringExpr = Expr {
    desc = Desc {
        _name = "$string"
        , _toStr = "$string"
        , _hash = hashWithName "$string" []
        , _params = []
        , _hasVar = True
    }
    , eval = \l -> case l of
        (Parsers.String s) -> return $ s
        _ -> throwError "not a string"
}

varBytesExpr :: Expr Bytes
varBytesExpr = Expr {
    desc = Desc {
        _name = "$bytes"
        , _toStr = "$bytes"
        , _hash = hashWithName "$bytes" []
        , _params = []
        , _hasVar = True
    }
    , eval = \l -> case l of
        (Parsers.String s) -> return $ s
        _ -> throwError "not bytes"
}