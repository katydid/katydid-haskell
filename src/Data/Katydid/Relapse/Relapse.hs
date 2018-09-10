-- |
-- This module provides an implementation of the relapse validation language.
--
-- Relapse is intended to be used for validation of trees or filtering of lists of trees.
--
-- Katydid currently provides two types of trees out of the box: Json and XML, 
-- but relapse supports any type of tree as long the type 
-- is of the Tree typeclass provided by the Parsers module.
--
-- The validate and filter functions expects a Tree to be a list of trees, 
-- since not all serialization formats have a single root.
-- For example, valid json like "[1, 2]" does not have a single root.
-- Relapse can also validate these types of trees.  
-- If your tree has a single root, simply provide a singleton list as input.

module Data.Katydid.Relapse.Relapse (
    parse, parseWithUDFs, Grammar
    , validate, filter
) where

import Prelude hiding (filter)
import Control.Monad.State (runState)
import Control.Monad (filterM)

import Data.Katydid.Parser.Parser

import qualified Data.Katydid.Relapse.Parser as Parser
import qualified Data.Katydid.Relapse.Ast as Ast
import qualified Data.Katydid.Relapse.MemDerive as MemDerive
import qualified Data.Katydid.Relapse.Smart as Smart
import qualified Data.Katydid.Relapse.Exprs as Exprs

-- | Grammar represents a compiled relapse grammar.
newtype Grammar = Grammar Smart.Grammar

-- |
-- parse parses the relapse grammar and returns either a parsed grammar or an error string.
parse :: String -> Either String Grammar
parse grammarString = do {
    parsed <- Parser.parseGrammar grammarString;
    Grammar <$> Smart.compile parsed;
}

-- |
-- parseWithUDFs parses the relapse grammar with extra user defined functions
-- and returns either a parsed grammar or an error string.
parseWithUDFs :: Exprs.MkFunc -> String -> Either String Grammar
parseWithUDFs userLib grammarString = do {
    parsed <- Parser.parseGrammarWithUDFs userLib grammarString;
    Grammar <$> Smart.compile parsed;
}

-- |
-- validate returns whether a tree is valid, given the grammar.
validate :: Tree t => Grammar -> [t] -> Bool
validate g tree = case filter g [tree] of
    [] -> False
    _ -> True

-- |
-- filter returns a filtered list of trees, given the grammar.
filter :: Tree t => Grammar -> [[t]] -> [[t]]
filter (Grammar g) trees = 
    let start = Smart.lookupMain g
        f = filterM (MemDerive.validate g start) trees
        (r, _) = runState f MemDerive.newMem
    in r
