module Main where

import Lib
import qualified Data.Map.Strict as Map

get :: Maybe String -> String
get (Just s) = s
get Nothing = ""

main :: IO ()
main = let
	m = Map.empty
	m1 = Map.insert 0 "a" m
	m2 = Map.insert 0 "b" m1
	in putStrLn $ get $ Map.lookup 0 m2
