module Exprs.Var (
    varBoolExpr
    , varIntExpr
    , varUintExpr
    , varDoubleExpr
    , varStringExpr
    , varBytesExpr
    , isVar
) where

import Control.Monad.Except (throwError)
import Data.Text (Text)
import Data.ByteString (ByteString)

import qualified Parsers
import Expr

isVar :: Desc -> Bool
isVar d = length (_params d) == 0 && case _name d of
    "$bool" -> True
    "$int" -> True
    "$uint" -> True
    "$double" -> True
    "$string" -> True
    "$[]byte" -> True
    _ -> False

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
        (Parsers.Int i) -> return i
        _ -> throwError "not an int"
}

varUintExpr :: Expr Word
varUintExpr = Expr {
    desc = Desc {
        _name = "$uint"
        , _toStr = "$uint"
        , _hash = hashWithName "$uint" []
        , _params = []
        , _hasVar = True
    }
    , eval = \l -> case l of
        (Parsers.Uint u) -> return u
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
        (Parsers.Double d) -> return d
        _ -> throwError "not a double"
}

varStringExpr :: Expr Text
varStringExpr = Expr {
    desc = Desc {
        _name = "$string"
        , _toStr = "$string"
        , _hash = hashWithName "$string" []
        , _params = []
        , _hasVar = True
    }
    , eval = \l -> case l of
        (Parsers.String s) -> return s
        _ -> throwError "not a string"
}

varBytesExpr :: Expr ByteString
varBytesExpr = Expr {
    desc = Desc {
        _name = "$[]byte"
        , _toStr = "$[]byte"
        , _hash = hashWithName "$[]byte" []
        , _params = []
        , _hasVar = True
    }
    , eval = \l -> case l of
        (Parsers.Bytes b) -> return b
        _ -> throwError "not bytes"
}