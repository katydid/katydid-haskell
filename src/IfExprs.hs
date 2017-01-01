module IfExprs where

import Patterns

data IfExprs
	= Cond {
		cond :: Value
		, thn :: IfExprs
		, els :: IfExprs
	}
	| Ret [Pattern]

compileIfExprs :: [IfExpr] -> IfExprs
compileIfExprs [] = Ret []
compileIfExprs (e:es) = 
	let ifs = compileIfExprs es
	in addIfExpr (simplifyIf e) ifs

evalIfExprs :: IfExprs -> ValueType -> [Pattern]
evalIfExprs (Ret ps) _ = ps
evalIfExprs (Cond c t e) v = if eval c v
	then evalIfExprs t v
	else evalIfExprs e v

-- TODO add simplifyPattern
simplifyIf :: IfExpr -> IfExpr
simplifyIf (cond, thn, els) = (simplifyValue cond, thn, els)

addIfExpr :: IfExpr -> IfExprs -> IfExprs
addIfExpr (c, t, e) (Ret ps) =
	Cond c (Ret (t:ps)) (Ret (e:ps))
addIfExpr (c, t, e) (Cond cs ts es) =
	if c == cs
	then Cond cs (addRet t ts) (addRet e es)
	else if isFalse (simplifyValue (AndValue c cs))
		then Cond cs (addRet e ts) (addIfExpr (c, t, e) es)
		else if isFalse (simplifyValue (AndValue (NotValue c) cs))
			then Cond cs (addIfExpr (c, t, e) ts) (addRet t es)
			else Cond cs (addIfExpr (c, t, e) ts) (addIfExpr (c, t, e) es)

addRet :: Pattern -> IfExprs -> IfExprs
addRet p (Ret ps) = Ret (p:ps)
addRet p (Cond c thn els) = Cond c (addRet p thn) (addRet p els)
