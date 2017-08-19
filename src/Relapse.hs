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

module Relapse (
    parseGrammar, validate, filter
) where

import Prelude hiding (filter)
import Control.Monad.Except (Except, throwError, return)
import Control.Monad.State (runState)
import Control.Monad (filterM)

import qualified Parser
import Patterns (Refs)
import qualified Patterns
import qualified MemDerive
import Parsers

-- |
-- parseGrammar parses the relapse grammar and returns either a parsed grammar (Refs, for the list of references) or an error string.
parseGrammar :: String -> Except String Refs
parseGrammar grammarString = case Parser.parseGrammar grammarString of
    (Left l) -> throwError (show l)
    (Right r) -> return r

-- |
-- validate returns whether a tree is valid, given the grammar (Refs).
validate :: Tree t => Refs -> [t] -> Bool
validate refs tree = case filter refs [tree] of
    [] -> False
    _ -> True

-- |
-- filter returns a filtered list of trees, given the grammar (Refs).
filter :: Tree t => Refs -> [[t]] -> [[t]]
filter refs trees = 
    let start = Patterns.lookupRef refs "main"
        f = filterM (MemDerive.validate refs start) trees
        (r, _) = runState f MemDerive.newMem
    in r
