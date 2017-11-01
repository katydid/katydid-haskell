-- |
-- This module describes the patterns supported by Relapse.
--
-- It also contains some simple functions for the map of references that a Relapse grammar consists of.
--
-- Finally it also contains some very simple pattern functions.
module Patterns (
    Pattern(..), 
    Refs, emptyRef, union, newRef, reverseLookupRef, lookupRef, hasRecursion,
    nullable, unescapable
) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Expr

-- |
-- Pattern recursively describes a Relapse Pattern.
data Pattern
    = Empty
    | ZAny
    | Node (Expr Bool) Pattern
    | Or Pattern Pattern
    | And Pattern Pattern
    | Not Pattern
    | Concat Pattern Pattern
    | Interleave Pattern Pattern
    | ZeroOrMore Pattern
    | Optional Pattern
    | Contains Pattern
    | Reference String
    deriving (Eq, Ord, Show)

-- |
-- The nullable function returns whether a pattern is nullable.
-- This means that the pattern matches the empty string.
nullable :: Refs -> Pattern -> Bool
nullable _ Empty = True
nullable _ ZAny = True
nullable _ Node{} = False
nullable refs (Or l r) = nullable refs l || nullable refs r
nullable refs (And l r) = nullable refs l && nullable refs r
nullable refs (Not p) = not $ nullable refs p
nullable refs (Concat l r) = nullable refs l && nullable refs r
nullable refs (Interleave l r) = nullable refs l && nullable refs r
nullable _ (ZeroOrMore _) = True
nullable _ (Optional _) = True
nullable refs (Contains p) = nullable refs p
nullable refs (Reference name) = nullable refs $ lookupRef refs name

-- |
-- unescapable is used for short circuiting.
-- A part of the tree can be skipped if all patterns are unescapable.
unescapable :: Pattern -> Bool
unescapable ZAny = True
unescapable (Not ZAny) = True
unescapable _ = False

-- |
-- Refs is a map from reference name to pattern and describes a relapse grammar.
newtype Refs = Refs (M.Map String Pattern)
    deriving (Show, Eq)

-- |
-- lookupRef looks up a pattern in the reference map, given a reference name.
lookupRef :: Refs -> String -> Pattern
lookupRef (Refs m) name = m M.! name

-- |
-- reverseLookupRef returns the reference name for a given pattern.
reverseLookupRef :: Pattern -> Refs -> Maybe String
reverseLookupRef p (Refs m) = case M.keys $ M.filter (== p) m of
    []      -> Nothing
    (k:_)  -> Just k

-- |
-- newRef returns a new reference map given a single pattern and its reference name.
newRef :: String -> Pattern -> Refs
newRef key value = Refs $ M.singleton key value

-- |
-- emptyRef returns an empty reference map.
emptyRef :: Refs
emptyRef = Refs M.empty

-- |
-- union returns the union of two reference maps.
union :: Refs -> Refs -> Refs
union (Refs m1) (Refs m2) = Refs $ M.union m1 m2 

-- |
-- hasRecursion returns whether an relapse grammar has any recursion, starting from the "main" reference.
hasRecursion :: Refs -> Bool
hasRecursion refs = hasRec refs (S.singleton "main") (lookupRef refs "main")

hasRec :: Refs -> S.Set String -> Pattern -> Bool
hasRec _ _ Empty = False
hasRec _ _ ZAny = False
hasRec _ _ Node{} = False
hasRec refs set (Or l r) = hasRec refs set l || hasRec refs set r
hasRec refs set (And l r) = hasRec refs set l || hasRec refs set r
hasRec refs set (Not p) = hasRec refs set p
hasRec refs set (Concat l r) = hasRec refs set l || (nullable refs l && hasRec refs set r)
hasRec refs set (Interleave l r) = hasRec refs set l || hasRec refs set r
hasRec _ _ (ZeroOrMore _) = False
hasRec refs set (Optional p) = hasRec refs set p
hasRec refs set (Contains p) = hasRec refs set p
hasRec refs set (Reference name) = S.member name set || hasRec refs (S.insert name set) (lookupRef refs name)
