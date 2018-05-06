{-# LANGUAGE FlexibleInstances #-}

-- | 
-- This module DeriveSpec tests the Derive module.
module DeriveSpec (
    tests
) where

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as HUnit

import Data.Tree
import qualified Derive
import qualified Parser
import qualified Smart
import qualified Parsers

import Data.List.Index (imap)

instance Parsers.Tree (Tree Parsers.Label) where
    getLabel (Node l _) = l
    getChildren (Node _ cs) = cs

tests = T.testGroup "Derive" [
    T.testGroup "derive" [
        HUnit.testCase "two ors" $
            either HUnit.assertFailure (\(want,got) -> HUnit.assertEqual "(want,got)" want got) $ do {
                input <- Parser.parseGrammar "(== 1 | !(== 2))" >>= Smart.compile;
                want <- Parser.parseGrammar "*" >>= Smart.compile;
                got <- Derive.derive input [Node (Parsers.Int 1) []];
                return (Smart.lookupMain want, got)
            }
        , HUnit.testCase "two interleaves" $
            either HUnit.assertFailure (\(want,got) -> HUnit.assertEqual "(want,got)" want got) $ do {
                input <- Parser.parseGrammar "{== 1 ; !(== 2)}" >>= Smart.compile;
                want <- Parser.parseGrammar "({<empty>;!(==2)}|{==1;*})" >>= Smart.compile;
                got <- Derive.derive input [Node (Parsers.Int 1) []];
                return (Smart.lookupMain want, got)
            }
    ]
    , T.testGroup "removeOneForEach" [
        HUnit.testCase "[1,2]" $
            HUnit.assertEqual "1,2" [[2],[1]] $ Derive.removeOneForEach [1,2]
        , HUnit.testCase "[1,2,3]" $
            HUnit.assertEqual "1,2,3" [[2,3],[1,3],[1,2]] $ Derive.removeOneForEach [1,2,3]
        , HUnit.testCase "[1]" $
            HUnit.assertEqual "1" [[]] $ Derive.removeOneForEach [1]
    ]]