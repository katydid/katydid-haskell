module Main where

import UnsafeMem
import MapDeriv
import Deriv
import Data.Map
import UnsafeDeriv

main :: IO ()
main = let
	m = empty
	m1 = insert 0 "test 123" m
	m2 = insert 0 "test 456" m1
	in putStrLn $ m2 ! 0
