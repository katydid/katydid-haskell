-- |
-- This is an internal relapse module.
--
-- It contains multiple implementations of if expressions.

module Data.Katydid.Relapse.IfExprs
  ( IfExprs
  , IfExpr
  , newIfExpr
  , evalIfExprs
  , compileIfExprs
  , ZippedIfExprs
  , zipIfExprs
  , evalZippedIfExprs
  )
where

import           Data.Katydid.Parser.Parser

import           Data.Katydid.Relapse.Smart
import           Data.Katydid.Relapse.Expr
import           Data.Katydid.Relapse.Exprs.Logic
import           Data.Katydid.Relapse.Simplify
import           Data.Katydid.Relapse.Zip

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
compileIfExprs :: [IfExpr] -> IfExprs
compileIfExprs []                   = Ret []
compileIfExprs (IfExpr ifExpr : es) = addIfExpr ifExpr (compileIfExprs es)

-- | valIfExprs evaluates a tree of if expressions and returns the resulting patterns or an error.
evalIfExprs :: IfExprs -> Label -> Either String [Pattern]
evalIfExprs (Ret ps    ) _ = return ps
evalIfExprs (Cond c t e) l = do
  b <- eval c l
  if b then evalIfExprs t l else evalIfExprs e l

addIfExpr :: (Expr Bool, Pattern, Pattern) -> IfExprs -> IfExprs
addIfExpr (c, t, e) (Ret ps) = Cond c (Ret (t : ps)) (Ret (e : ps))
addIfExpr (c, t, e) (Cond cs ts es)
  | c == cs = Cond cs (addRet t ts) (addRet e es)
  | boolExpr False == andExpr c cs = Cond cs
                                          (addRet e ts)
                                          (addIfExpr (c, t, e) es)
  | boolExpr False == andExpr (notExpr c) cs = Cond cs
                                                    (addIfExpr (c, t, e) ts)
                                                    (addRet t es)
  | otherwise = Cond cs (addIfExpr (c, t, e) ts) (addIfExpr (c, t, e) es)

addRet :: Pattern -> IfExprs -> IfExprs
addRet p (Ret ps    ) = Ret (p : ps)
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
zipIfExprs (Ret ps    ) = let (zps, zs) = zippy ps in ZippedRet zps zs

-- | evalZippedIfExprs evaulates a ZippedIfExprs tree and returns the zipped pattern list and zipper from the resulting leaf.
evalZippedIfExprs :: ZippedIfExprs -> Label -> Either String ([Pattern], Zipper)
evalZippedIfExprs (ZippedRet ps zs ) _ = return (ps, zs)
evalZippedIfExprs (ZippedCond c t e) v = do
  b <- eval c v
  if b then evalZippedIfExprs t v else evalZippedIfExprs e v

