module Main where

import Lib
import qualified Data.Map.Strict as Map
import qualified Data.Tree as Tree

type ValueType = String

data Value
	= Equal ValueType
	| OrValue Value Value
	| ExceptValue Value
	| AnyValue
	deriving (Eq, Ord)

eval :: Value -> ValueType -> Bool
eval (Equal value) v = v == value
eval (OrValue l r) v = eval l v || eval r v
eval (ExceptValue value) v = not $ eval value v
eval AnyValue _ = True

type IfExpr = (Value, Pattern, Pattern)

evalIf :: IfExpr -> ValueType -> Pattern
evalIf (value, thn, els) v = 
	if eval value v then thn else els

data Pattern
	= Empty
	| ZAny
	| Node Value Pattern
	| Or Pattern Pattern
	| And Pattern Pattern
	| Not Pattern
	| Concat Pattern Pattern
	| Interleave Pattern Pattern
	| ZeroOrMore Pattern
	| Optional Pattern
	| Contains Pattern
	| Reference String
	deriving (Eq, Ord)

type Refs = Map.Map String Pattern

nullable :: Refs -> Pattern -> Bool
nullable refs Empty = True
nullable refs ZAny = True
nullable refs (Node _ _) = False
nullable refs (Or l r) = nullable refs l || nullable refs r
nullable refs (And l r) = nullable refs l && nullable refs r
nullable refs (Not p) = not $ nullable refs p
nullable refs (Concat l r) = nullable refs l && nullable refs r
nullable refs (Interleave l r) = nullable refs l && nullable refs r
nullable refs (ZeroOrMore _) = True
nullable refs (Optional _) = True
nullable refs (Contains p) = nullable refs p
nullable refs (Reference name) = case Map.lookup name refs of
	(Just p) -> nullable refs p
	Nothing  -> error $ "reference <" ++ name ++ "> does not exist"

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
derivCall refs (Reference name) = case Map.lookup name refs of
	(Just p) -> derivCall refs p
	Nothing  -> error $ "reference <" ++ name ++ "> does not exist"
derivCall refs (Not p) = derivCall refs p
derivCall refs (Contains p) = derivCall refs p
derivCall refs (Optional p) = derivCall refs p

derivReturns :: Refs -> [Pattern] -> [Bool] -> [Pattern]
derivReturns refs [] [] = []
derivReturns refs (p:tailps) ns =
	let (dp, tailns) = derivReturn refs p ns
	in  dp:(derivReturns refs tailps tailns)

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
derivReturn refs (Reference name) ns = case Map.lookup name refs of
	(Just p) -> derivReturn refs p ns
	Nothing  -> error $ "reference <" ++ name ++ "> does not exist"
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
	let	ifs = derivCalls refs ps
		childps = map (flip evalIf label) ifs
		childres = foldl (deriv refs) childps children
		childns = map (nullable refs) childres
	in derivReturns refs ps childns

type MemCalls = Map.Map [Pattern] [IfExpr]

memCalls :: MemCalls -> Refs -> [Pattern] -> (MemCalls, [IfExpr])
memCalls m refs ps = case Map.lookup ps m of
	(Just ifs) -> (m, ifs)
	Nothing    -> let ifs = derivCalls refs ps
		in (Map.insert ps ifs m, ifs)

type MemReturns = Map.Map ([Pattern], [Bool]) [Pattern]

memReturns :: MemReturns -> Refs -> [Pattern] -> [Bool] -> (MemReturns, [Pattern])
memReturns m refs ps ns = case Map.lookup (ps, ns) m of
	(Just ps) -> (m, ps)
	Nothing   -> let res = derivReturns refs ps ns
		in (Map.insert (ps, ns) res m, res)

mderiv :: MemCalls -> MemReturns -> Refs -> [Pattern] -> Tree.Tree ValueType -> (MemCalls, MemReturns, [Pattern])
mderiv mc mr refs ps (Tree.Node label children) =
	let	(mc', ifs) = memCalls mc refs ps
		childps = map (flip evalIf label) ifs
		(mc'', mr', childres) = foldl (foldableMderiv refs) (mc', mr, childps) children
		childns = map (nullable refs) childres
		(mr'', res) = memReturns mr' refs ps childns
	in (mc'', mr'', res)

foldableMderiv :: Refs -> (MemCalls, MemReturns, [Pattern]) -> Tree.Tree ValueType -> (MemCalls, MemReturns, [Pattern])
foldableMderiv refs (mc, mr, ps) n = mderiv mc mr refs ps n

-- unescapable is used for short circuiting.
-- A part of the tree can be skipped if all patterns are unescapable.
unescapable :: Pattern -> Bool
unescapable ZAny = True
unescapable (Not ZAny) = True
unescapable _ = False

get :: Maybe String -> String
get (Just s) = s
get Nothing = ""

main :: IO ()
main = let
	m = Map.empty
	m1 = Map.insert 0 "a" m
	m2 = Map.insert 0 "b" m1
	in putStrLn $ get $ Map.lookup 0 m2
