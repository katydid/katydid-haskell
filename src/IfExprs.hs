module IfExprs where

import Patterns
import Values
import Simplify
import Zip

type IfExpr = (Value, Pattern, Pattern)

evalIf :: ValueType -> IfExpr -> Pattern
evalIf v (value, thn, els) = 
	if eval value v then thn else els

data IfExprs
	= Cond {
		cond :: Value
		, thn :: IfExprs
		, els :: IfExprs
	}
	| Ret [Pattern]

compileIfExprs :: Refs -> [IfExpr] -> IfExprs
compileIfExprs _ [] = Ret []
compileIfExprs refs (e:es) = addIfExpr (simplifyIf refs e) (compileIfExprs refs es)

evalIfExprs :: IfExprs -> ValueType -> [Pattern]
evalIfExprs (Ret ps) _ = ps
evalIfExprs (Cond c t e) v 
	| eval c v  = evalIfExprs t v
	| otherwise = evalIfExprs e v

simplifyIf :: Refs -> IfExpr -> IfExpr
simplifyIf refs (cond, thn, els) = 
	let	scond = simplifyValue cond
		sthn  = simplify refs thn
		sels  = simplify refs els
	in if sthn == sels then (AnyValue, sthn, sels) else (scond, sthn, sels)

addIfExpr :: IfExpr -> IfExprs -> IfExprs
addIfExpr (c, t, e) (Ret ps) =
	Cond c (Ret (t:ps)) (Ret (e:ps))
addIfExpr (c, t, e) (Cond cs ts es)
	| c == cs = Cond cs (addRet t ts) (addRet e es)
	| (NotValue AnyValue) == (simplifyValue (AndValue c cs)) = Cond cs (addRet e ts) (addIfExpr (c, t, e) es)
	| (NotValue AnyValue) == (simplifyValue (AndValue (NotValue c) cs)) = Cond cs (addIfExpr (c, t, e) ts) (addRet t es)
	| otherwise = Cond cs (addIfExpr (c, t, e) ts) (addIfExpr (c, t, e) es)

addRet :: Pattern -> IfExprs -> IfExprs
addRet p (Ret ps) = Ret (p:ps)
addRet p (Cond c thn els) = Cond c (addRet p thn) (addRet p els)

data ZippedIfExprs
	= ZippedCond {
		zcond :: Value
		, zthn :: ZippedIfExprs
		, zels :: ZippedIfExprs
	}
	| ZippedRet [Pattern] Zipper

zipIfExprs :: IfExprs -> ZippedIfExprs
zipIfExprs (Cond c t e) = ZippedCond c (zipIfExprs t) (zipIfExprs e)
zipIfExprs (Ret ps) = let (ps, zs) = zippy ps in ZippedRet ps zs

evalZippedIfExprs :: ZippedIfExprs -> ValueType -> ([Pattern], Zipper)
evalZippedIfExprs (ZippedRet ps zs) _ = (ps, zs)
evalZippedIfExprs (ZippedCond c t e) v
	| eval c v  = evalZippedIfExprs t v
	| otherwise = evalZippedIfExprs e v


