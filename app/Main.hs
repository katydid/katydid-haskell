module Main where

import UnsafeMem
import MapDeriv
import Deriv
import Data.Map
import UnsafeDeriv

-- unescapable is used for short circuiting.
-- A part of the tree can be skipped if all patterns are unescapable.
unescapable :: Pattern -> Bool
unescapable ZAny = True
unescapable (Not ZAny) = True
unescapable _ = False

main :: IO ()
main = let
	m = empty
	m1 = insert 0 "a" m
	m2 = insert 0 "b" m1
	in putStrLn $ m2 ! 0
