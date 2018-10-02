{-# LANGUAGE FlexibleInstances #-}

-- |
-- This module runs the relapse parsing and validation tests.
module Main where

import qualified Test.Tasty                    as T
import qualified Test.Tasty.HUnit              as HUnit

import qualified ParserSpec
import qualified Suite
import qualified RelapseSpec
import qualified DeriveSpec
import qualified Data.Katydid.Parser.Protobuf.Descriptor
import qualified Data.Katydid.Parser.Protobuf.Protobuf

main :: IO ()
main = do
    testSuiteCases <- Suite.readTestCases
    T.defaultMain $ T.testGroup
        "Tests"
        [ ParserSpec.tests
        , RelapseSpec.tests
        , Suite.tests testSuiteCases
        , DeriveSpec.tests
        , Data.Katydid.Parser.Protobuf.Descriptor.tests
        , Data.Katydid.Parser.Protobuf.Protobuf.tests
        ]
