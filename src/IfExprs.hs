-- |
-- This is an internal relapse module.
--
-- It contains multiple implementations of if expressions.

module IfExprs (
    IfExprs, IfExpr, newIfExpr,
    evalIfExprs, compileIfExprs,
    ZippedIfExprs, zipIfExprs, evalZippedIfExprs
) where

import Patterns
import Expr
import Exprs.Logic
import Simplify
import Zip
import Parsers

-- |
-- IfExpr contains a condition and a return pattern for each of the two cases.
newtype IfExpr = IfExpr (Expr Bool, Pattern, Pattern)

-- |
-- newIfExpr creates an IfExpr.
newIfExpr :: Expr Bool -> Pattern -> Pattern -> IfExpr
newIfExpr c t e = IfExpr (c, t, e)

-- | IfExprs is a tree of if expressions, which contains a list of resulting patterns on each of its leaves.
data IfExprs
    = Cond {
        cond :: Expr Bool
        , thn :: IfExprs
        , els :: IfExprs
    }
    | Ret [Pattern]

-- | compileIfExprs compiles a list of if expressions in an IfExprs tree, for efficient evaluation.
compileIfExprs :: Refs -> [IfExpr] -> IfExprs
compileIfExprs _ [] = Ret []
compileIfExprs refs (e:es) = let (IfExpr ifExpr) = simplifyIf refs e
    in addIfExpr ifExpr (compileIfExprs refs es)

-- | valIfExprs evaluates a tree of if expressions and returns the resulting patterns or an error.
evalIfExprs :: IfExprs -> Label -> Either String [Pattern]
evalIfExprs (Ret ps) _ = return ps
evalIfExprs (Cond c t e) l = do {
    b <- eval c l;
    if b then evalIfExprs t l else evalIfExprs e l
}

simplifyIf :: Refs -> IfExpr -> IfExpr
simplifyIf refs (IfExpr (c, t, e)) =
    let scond = c
        sthn  = simplify refs t
        sels  = simplify refs e
    in if sthn == sels then IfExpr (boolExpr True, sthn, sels) else IfExpr (scond, sthn, sels)

addIfExpr :: (Expr Bool, Pattern, Pattern) -> IfExprs -> IfExprs
addIfExpr (c, t, e) (Ret ps) =
    Cond c (Ret (t:ps)) (Ret (e:ps))
addIfExpr (c, t, e) (Cond cs ts es)
    | c == cs = Cond cs (addRet t ts) (addRet e es)
    | boolExpr False == andExpr c cs = Cond cs (addRet e ts) (addIfExpr (c, t, e) es)
    | boolExpr False == andExpr (notExpr c) cs = Cond cs (addIfExpr (c, t, e) ts) (addRet t es)
    | otherwise = Cond cs (addIfExpr (c, t, e) ts) (addIfExpr (c, t, e) es)

addRet :: Pattern -> IfExprs -> IfExprs
addRet p (Ret ps) = Ret (p:ps)
addRet p (Cond c t e) = Cond c (addRet p t) (addRet p e)

-- |
-- ZippedIfExprs is a tree of if expressions, but with a zipped pattern list and a zipper on each of the leaves.
data ZippedIfExprs
    = ZippedCond {
        zcond :: Expr Bool
        , zthn :: ZippedIfExprs
        , zels :: ZippedIfExprs
    }
    | ZippedRet [Pattern] Zipper

-- | zipIfExprs compresses an if expression tree's leaves.
zipIfExprs :: IfExprs -> ZippedIfExprs
zipIfExprs (Cond c t e) = ZippedCond c (zipIfExprs t) (zipIfExprs e)
zipIfExprs (Ret ps) = let (zps, zs) = zippy ps in ZippedRet zps zs

-- | evalZippedIfExprs evaulates a ZippedIfExprs tree and returns the zipped pattern list and zipper from the resulting leaf.
evalZippedIfExprs :: ZippedIfExprs -> Label -> Either String ([Pattern], Zipper)
evalZippedIfExprs (ZippedRet ps zs) _ = return (ps, zs)
evalZippedIfExprs (ZippedCond c t e) v = do {
    b <- eval c v;
    if b then evalZippedIfExprs t v else evalZippedIfExprs e v
}

