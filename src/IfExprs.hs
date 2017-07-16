module IfExprs where

import Patterns
import Values
import Simplify
import Zip
import Parsers
import Control.Monad.Except (Except, runExcept)

type IfExpr = (BoolExpr, Pattern, Pattern)

evalIf :: IfExpr -> Label -> Except ValueErr Pattern
evalIf (value, thn, els) l = do {
    b <- eval value l;
    return $ if b then thn else els
}

data IfExprs
	= Cond {
		cond :: BoolExpr
		, thn :: IfExprs
		, els :: IfExprs
	}
	| Ret [Pattern]

compileIfExprs :: Refs -> [IfExpr] -> IfExprs
compileIfExprs _ [] = Ret []
compileIfExprs refs (e:es) = addIfExpr (simplifyIf refs e) (compileIfExprs refs es)

evalIfExprs :: IfExprs -> Label -> Except ValueErr [Pattern]
evalIfExprs (Ret ps) _ = return ps
evalIfExprs (Cond c t e) l = do {
	b <- eval c l;
	if b then evalIfExprs t l else evalIfExprs e l
}

simplifyIf :: Refs -> IfExpr -> IfExpr
simplifyIf refs (cond, thn, els) = 
	let	scond = simplifyBoolExpr cond
		sthn  = simplify refs thn
		sels  = simplify refs els
	in if sthn == sels then (BoolConst True, sthn, sels) else (scond, sthn, sels)

addIfExpr :: IfExpr -> IfExprs -> IfExprs
addIfExpr (c, t, e) (Ret ps) =
	Cond c (Ret (t:ps)) (Ret (e:ps))
addIfExpr (c, t, e) (Cond cs ts es)
	| c == cs = Cond cs (addRet t ts) (addRet e es)
	| BoolConst False == simplifyBoolExpr (AndFunc c cs) = Cond cs (addRet e ts) (addIfExpr (c, t, e) es)
	| BoolConst False == simplifyBoolExpr (AndFunc (NotFunc c) cs) = Cond cs (addIfExpr (c, t, e) ts) (addRet t es)
	| otherwise = Cond cs (addIfExpr (c, t, e) ts) (addIfExpr (c, t, e) es)

addRet :: Pattern -> IfExprs -> IfExprs
addRet p (Ret ps) = Ret (p:ps)
addRet p (Cond c thn els) = Cond c (addRet p thn) (addRet p els)

data ZippedIfExprs
	= ZippedCond {
		zcond :: BoolExpr
		, zthn :: ZippedIfExprs
		, zels :: ZippedIfExprs
	}
	| ZippedRet [Pattern] Zipper

zipIfExprs :: IfExprs -> ZippedIfExprs
zipIfExprs (Cond c t e) = ZippedCond c (zipIfExprs t) (zipIfExprs e)
zipIfExprs (Ret ps) = let (zps, zs) = zippy ps in ZippedRet zps zs

evalZippedIfExprs :: ZippedIfExprs -> Label -> Except ValueErr ([Pattern], Zipper)
evalZippedIfExprs (ZippedRet ps zs) _ = return (ps, zs)
evalZippedIfExprs (ZippedCond c t e) v = do {
	b <- eval c v;
	if b then evalZippedIfExprs t v else evalZippedIfExprs e v
}

