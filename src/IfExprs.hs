module IfExprs where

import Patterns
import Values
import Simplify
import Zip
import Parsers

type IfExpr = (BoolExpr, Pattern, Pattern)

evalIf :: Label -> IfExpr -> Value Pattern
evalIf v (value, thn, els) = do {
	b <- eval value v;
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

evalIfExprs :: Label -> IfExprs -> Value [Pattern]
evalIfExprs _ (Ret ps) = Value ps
evalIfExprs v (Cond c t e) = do {
	b <- eval c v;
	if b then evalIfExprs v t else evalIfExprs v e
}

mustEvalIf :: IfExprs -> Label -> [Pattern]
mustEvalIf ifs l = case evalIfExprs l ifs of
    (Err e) -> error e
    (Value v) -> v

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
	| (BoolConst False) == (simplifyBoolExpr (AndFunc c cs)) = Cond cs (addRet e ts) (addIfExpr (c, t, e) es)
	| (BoolConst False) == (simplifyBoolExpr (AndFunc (NotFunc c) cs)) = Cond cs (addIfExpr (c, t, e) ts) (addRet t es)
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
zipIfExprs (Ret ps) = let (ps, zs) = zippy ps in ZippedRet ps zs

evalZippedIfExprs :: ZippedIfExprs -> Label -> Value ([Pattern], Zipper)
evalZippedIfExprs (ZippedRet ps zs) _ = Value (ps, zs)
evalZippedIfExprs (ZippedCond c t e) v = do {
	b <- eval c v;
	if b then evalZippedIfExprs t v else evalZippedIfExprs e v
}


