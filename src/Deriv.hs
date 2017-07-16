module Deriv (
    derivs, derivCalls, derivReturns, zipderivs
) where

import Data.Foldable (foldlM)
import Control.Monad.Except (Except, runExcept, mapExcept, throwError)

import Patterns
import Values
import Parsers
import Simplify
import Zip
import IfExprs

derivCalls :: Refs -> [Pattern] -> IfExprs
derivCalls refs ps = compileIfExprs refs $ concatMap (derivCall refs) ps

derivCall :: Refs -> Pattern -> [IfExpr]
derivCall _ Empty = []
derivCall _ ZAny = []
derivCall _ (Node v p) = [(v, p, Not ZAny)]
derivCall refs (Concat l r) = if nullable refs l
    then derivCall refs l ++ derivCall refs r
    else derivCall refs l
derivCall refs (Or l r) = derivCall refs l ++ derivCall refs r
derivCall refs (And l r) = derivCall refs l ++ derivCall refs r
derivCall refs (Interleave l r) = derivCall refs l ++ derivCall refs r
derivCall refs (ZeroOrMore p) = derivCall refs p
derivCall refs (Reference name) = derivCall refs $ lookupRef refs name
derivCall refs (Not p) = derivCall refs p
derivCall refs (Contains p) = derivCall refs (Concat ZAny (Concat p ZAny))
derivCall refs (Optional p) = derivCall refs (Or p Empty)

derivReturns :: Refs -> ([Pattern], [Bool]) -> [Pattern]
derivReturns refs ([], []) = []
derivReturns refs (p:tailps, ns) =
    let (dp, tailns) = derivReturn refs p ns
        sp = simplify refs dp
    in  sp:derivReturns refs (tailps, tailns)

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
derivReturn refs z@(ZeroOrMore p) ns = 
    let (derivp, tailns) = derivReturn refs p ns
    in  (Concat derivp z, tailns)
derivReturn refs (Reference name) ns = derivReturn refs (lookupRef refs name) ns
derivReturn refs (Not p) ns =
    let (derivp, tailns) = derivReturn refs p ns
    in  (Not derivp, tailns)
derivReturn refs (Contains p) ns = derivReturn refs (Concat ZAny (Concat p ZAny)) ns
derivReturn refs (Optional p) ns = derivReturn refs (Or p Empty) ns

onePattern :: Either ValueErr [Pattern] -> Either String Pattern
onePattern (Right [r]) = return r
onePattern (Left e) = throwError $ show e
onePattern (Right rs) = throwError $ "Number of patterns is not one, but " ++ show rs

derivs :: Tree t => Refs -> [t] -> Except String Pattern
derivs g ts = mapExcept onePattern $ foldlM (deriv g) [lookupRef g "main"] ts

deriv :: Tree t => Refs -> [Pattern] -> t -> Except ValueErr [Pattern]
deriv refs ps tree =
    if all unescapable ps then return ps else
    let ifs = derivCalls refs ps
        d = deriv refs
        nulls = map (nullable refs)
    in do {
        childps <- evalIfExprs ifs (getLabel tree);
        childres <- foldlM d childps (getChildren tree);
        return $ derivReturns refs (ps, nulls childres);
    }

zipderivs :: Tree t => Refs -> [t] -> Except String Pattern
zipderivs g ts = mapExcept onePattern $ foldlM (zipderiv g) [lookupRef g "main"] ts

zipderiv :: Tree t => Refs -> [Pattern] -> t -> Except ValueErr [Pattern]
zipderiv refs ps tree =
    if all unescapable ps then return ps else
    let ifs = derivCalls refs ps
        d = zipderiv refs
        nulls = map (nullable refs)
    in do {
        childps <- evalIfExprs ifs (getLabel tree);
        (zchildps, zipper) <- return $ zippy childps;
        childres <- foldlM d zchildps (getChildren tree);
        let unzipns = unzipby zipper (nulls childres)
        in return $ derivReturns refs (ps, unzipns)
    }
