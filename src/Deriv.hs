module Deriv where

import qualified Data.Map.Strict as Map
import qualified Data.Tree as Tree
import Patterns
import Values
import Zip
import IfExprs

derivCalls :: Refs -> [Pattern] -> [IfExpr]
derivCalls refs ps = concatMap (derivCall refs) ps

derivCall :: Refs -> Pattern -> [IfExpr]
derivCall _ Empty = []
derivCall _ ZAny = []
derivCall _ (Node v p) = [(v, p, Not ZAny)]
derivCall refs (Concat l r) = if nullable refs l
	then (derivCall refs l) ++ (derivCall refs r)
	else derivCall refs l
derivCall refs (Or l r) = (derivCall refs l) ++ (derivCall refs r)
derivCall refs (And l r) = (derivCall refs l) ++ (derivCall refs r)
derivCall refs (Interleave l r) = (derivCall refs l) ++ (derivCall refs r)
derivCall refs (ZeroOrMore p) = derivCall refs p
derivCall refs (Reference name) = derivCall refs $ refs Map.! name
derivCall refs (Not p) = derivCall refs p
derivCall refs (Contains p) = derivCall refs p
derivCall refs (Optional p) = derivCall refs p

derivReturns :: Refs -> ([Pattern], [Bool]) -> [Pattern]
derivReturns refs ([], []) = []
derivReturns refs ((p:tailps), ns) =
	let (dp, tailns) = derivReturn refs p ns
	in  dp:(derivReturns refs (tailps, tailns))

derivReturn :: Refs -> Pattern -> [Bool] -> (Pattern, [Bool])
derivReturn refs Empty ns = (Not ZAny, ns)
derivReturn refs ZAny ns = (ZAny, ns)
derivReturn refs (Node v p) ns = if head ns 
	then (Empty, tail ns)
	else (Not ZAny, tail ns)
derivReturn refs (Concat l r) ns = 
	let	(leftDeriv, leftTail) = derivReturn refs l ns
		(rightDeriv, rightTail) = derivReturn refs r leftTail
		leftConcat = Concat leftDeriv r
	in if not $ nullable refs l
	then (Or leftConcat rightDeriv, rightTail)
	else (leftConcat, leftTail)
derivReturn refs (Or l r) ns = 
	let	(leftDeriv, leftTail) = derivReturn refs l ns
		(rightDeriv, rightTail) = derivReturn refs r leftTail
	in (Or leftDeriv rightDeriv, rightTail)
derivReturn refs (And l r) ns = 
	let	(leftDeriv, leftTail) = derivReturn refs l ns
		(rightDeriv, rightTail) = derivReturn refs r leftTail
	in (And leftDeriv rightDeriv, rightTail)
derivReturn refs (Interleave l r) ns = 
	let	(leftDeriv, leftTail) = derivReturn refs l ns
		(rightDeriv, rightTail) = derivReturn refs r leftTail
	in (Or (Interleave leftDeriv r) (Interleave rightDeriv l), rightTail)
derivReturn refs (ZeroOrMore p) ns = 
	let	(derivp, tailns) = derivReturn refs p ns
	in  (Concat derivp p, tailns)
derivReturn refs (Reference name) ns = derivReturn refs (refs Map.! name) ns
derivReturn refs (Not p) ns =
	let (derivp, tailns) = derivReturn refs p ns
	in  (Not derivp, tailns)
derivReturn refs (Contains p) ns =
	let (derivp, tailns) = derivReturn refs p ns
	in  (Contains derivp, tailns)
derivReturn refs (Optional p) ns =
	let (derivp, tailns) = derivReturn refs p ns
	in  (Optional derivp, tailns)

deriv :: Refs -> [Pattern] -> Tree.Tree ValueType -> [Pattern]
deriv refs ps (Tree.Node label children) =
	if all unescapable ps then ps else
	let	ifs = derivCalls refs ps
		childps = map (evalIf label) ifs
		childres = foldl (deriv refs) childps children
		childns = map (nullable refs) childres
	in derivReturns refs (ps, childns)

zipderiv :: Refs -> [Pattern] -> Tree.Tree ValueType -> [Pattern]
zipderiv refs ps (Tree.Node label children) =
	if all unescapable ps then ps else
	let	ifs = derivCalls refs ps
		childps = map (evalIf label) ifs
		(zipps, zipper) = zippy childps
		childres = foldl (zipderiv refs) zipps children
		childns = map (nullable refs) childres
		unzipns = unzipby zipper childns
	in derivReturns refs (ps, unzipns)

