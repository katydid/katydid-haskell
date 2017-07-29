module Zip (
    Zipper, zippy, unzipby
) where

import qualified Data.Set as DataSet
import Data.List (elemIndex)

import Patterns

type Zipper = [Int]

zippy :: [Pattern] -> ([Pattern], Zipper)
zippy ps =
    let s = DataSet.fromList ps
        s' = DataSet.delete ZAny s
        s'' = DataSet.delete (Not ZAny) s'
        l = DataSet.toAscList s''
    in (l, map (indexOf l) ps)

indexOf :: [Pattern] -> Pattern -> Int
indexOf _ ZAny = -1
indexOf _ (Not ZAny) = -2
indexOf ps p = case elemIndex p ps of
    (Just i) -> i

unzipby :: Zipper -> [Bool] -> [Bool]
unzipby z bs = map (ofIndexb bs) z

ofIndexb :: [Bool] -> Int -> Bool
ofIndexb _ (-1) = True
ofIndexb _ (-2) = False
ofIndexb bs i = bs !! i