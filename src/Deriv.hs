module Deriv where

import Debug.Trace
import Data.Foldable

import Patterns
import Values
import Parsers
import Simplify
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
derivCall refs (Reference name) = derivCall refs $ lookupRef refs name
derivCall refs (Not p) = derivCall refs p
derivCall refs (Contains p) = derivCall refs (Concat ZAny (Concat p ZAny))
derivCall refs (Optional p) = derivCall refs (Or p Empty)

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
    if nullable refs l
    then    let (leftDeriv, leftTail) = derivReturn refs l ns
                (rightDeriv, rightTail) = derivReturn refs r leftTail
            in  (Or (Concat leftDeriv r) rightDeriv, rightTail)
    else    let (leftDeriv, leftTail) = derivReturn refs l ns
            in  (Concat leftDeriv r, leftTail)
derivReturn refs (Or l r) ns = 
    let (leftDeriv, leftTail) = derivReturn refs l ns
        (rightDeriv, rightTail) = derivReturn refs r leftTail
    in (Or leftDeriv rightDeriv, rightTail)
derivReturn refs (And l r) ns = 
    let (leftDeriv, leftTail) = derivReturn refs l ns
        (rightDeriv, rightTail) = derivReturn refs r leftTail
    in (And leftDeriv rightDeriv, rightTail)
derivReturn refs (Interleave l r) ns = 
    let (leftDeriv, leftTail) = derivReturn refs l ns
        (rightDeriv, rightTail) = derivReturn refs r leftTail
    in (Or (Interleave leftDeriv r) (Interleave rightDeriv l), rightTail)
derivReturn refs (ZeroOrMore p) ns = 
    let (derivp, tailns) = derivReturn refs p ns
    in  (Concat derivp p, tailns)
derivReturn refs (Reference name) ns = derivReturn refs (lookupRef refs name) ns
derivReturn refs (Not p) ns =
    let (derivp, tailns) = derivReturn refs p ns
    in  (Not derivp, tailns)
derivReturn refs (Contains p) ns =
    let (derivp, tailns) = derivReturn refs (Concat ZAny (Concat p ZAny)) ns
    in  (Contains derivp, tailns)
derivReturn refs (Optional p) ns =
    let (derivp, tailns) = derivReturn refs (Or p Empty) ns
    in  (Optional derivp, tailns)

derivs :: Tree t => Refs -> [t] -> Value Pattern
derivs g ts = case foldlM (deriv g) [lookupRef g "main"] ts of
    (Value [r]) -> Value r
    (Err e) -> Err e
    (Value rs) -> error $ "Number of patterns is not one, but " ++ show rs

deriv :: Tree t => Refs -> [Pattern] -> t -> Value [Pattern]
deriv g ps t = let ret = deriv' g ps t
    in trace ("Deriv{\n\tLabel:" ++ show (getLabel t) ++ "\n\tInput:" ++ show ps ++ "\n\tOutput:" ++ show ret ++ "\n}" ) ret

deriv' :: Tree t => Refs -> [Pattern] -> t -> Value [Pattern]
deriv' refs ps tree =
    if all unescapable ps then Value ps else
    let ifs = derivCalls refs ps
    in do {
        childps <- mapM (evalIf (getLabel tree)) ifs;
        simpcs <- return $ map (simplify refs) childps;
        childres <- foldlM (deriv refs) simpcs (getChildren tree);
        childns <- return $ map (nullable refs) childres;
        res <- return $ derivReturns refs (ps, childns);
        return $ map (simplify refs) res
    }

-- zipderiv :: Tree t => Refs -> [Pattern] -> t -> [Pattern]
-- zipderiv refs ps tree =
--     if all unescapable ps then ps else
--     let ifs = derivCalls refs ps
--         compIfs = (compileIfExprs refs ifs)
--         childps = evalIfExprs compIfs (getLabel tree)
--         (zipps, zipper) = zippy childps
--         childres = foldl (zipderiv refs) zipps (getChildren tree)
--         childns = map (nullable refs) childres
--         unzipns = unzipby zipper childns
--     in derivReturns refs (ps, unzipns)

-- precall :: Refs -> [Pattern] -> ZippedIfExprs
-- precall refs current = 
--     let ifs = derivCalls refs current
--         compIfs = (compileIfExprs refs ifs)
--     in  zipIfExprs compIfs

-- thereturn :: Refs -> [Pattern] -> Zipper -> [Pattern] -> [Pattern]
-- thereturn refs current zipper childres =
--     let childns = map (nullable refs) childres
--         unzipns = unzipby zipper childns
--     in derivReturns refs (current, unzipns)

-- zipderiv2 :: Tree t => Refs -> [Pattern] -> t -> [Pattern]
-- zipderiv2 refs ps tree =
--     if all unescapable ps then ps else
--     let zifs = precall refs ps
--         (zipps, zipper) = evalZippedIfExprs zifs (getLabel tree)
--         childres = foldl (zipderiv2 refs) zipps (getChildren tree)
--     in thereturn refs ps zipper childres
