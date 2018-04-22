-- |
-- This module describes the Relapse's abstract syntax tree.
--
-- It also contains some simple functions for the map of references that a Relapse grammar consists of.
--
-- Finally it also contains some very simple pattern functions.
module Ast (
    Pattern(..), 
    Grammar, emptyRef, union, newRef, reverseLookupRef, lookupRef, hasRecursion,
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
nullable :: Grammar -> Pattern -> Bool
nullable _ Empty = True
nullable _ ZAny = True
nullable _ Node{} = False
nullable g (Or l r) = nullable g l || nullable g r
nullable g (And l r) = nullable g l && nullable g r
nullable g (Not p) = not $ nullable g p
nullable g (Concat l r) = nullable g l && nullable g r
nullable g (Interleave l r) = nullable g l && nullable g r
nullable _ (ZeroOrMore _) = True
nullable _ (Optional _) = True
nullable g (Contains p) = nullable g p
nullable g (Reference refName) = nullable g $ lookupRef g refName

-- |
-- unescapable is used for short circuiting.
-- A part of the tree can be skipped if all patterns are unescapable.
unescapable :: Pattern -> Bool
unescapable ZAny = True
unescapable (Not ZAny) = True
unescapable _ = False

-- |
-- Refs is a map from reference name to pattern and describes a relapse grammar.
newtype Grammar = Grammar (M.Map String Pattern)
    deriving (Show, Eq)

-- |
-- lookupRef looks up a pattern in the reference map, given a reference name.
lookupRef :: Grammar -> String -> Pattern
lookupRef (Grammar m) refName = m M.! refName

-- |
-- reverseLookupRef returns the reference name for a given pattern.
reverseLookupRef :: Pattern -> Grammar -> Maybe String
reverseLookupRef p (Grammar m) = case M.keys $ M.filter (== p) m of
    []      -> Nothing
    (k:_)  -> Just k

-- |
-- newRef returns a new reference map given a single pattern and its reference name.
newRef :: String -> Pattern -> Grammar
newRef key value = Grammar $ M.singleton key value

-- |
-- emptyRef returns an empty reference map.
emptyRef :: Grammar
emptyRef = Grammar M.empty

-- |
-- union returns the union of two reference maps.
union :: Grammar -> Grammar -> Grammar
union (Grammar m1) (Grammar m2) = Grammar $ M.union m1 m2 

-- |
-- hasRecursion returns whether an relapse grammar has any recursion, starting from the "main" reference.
hasRecursion :: Grammar -> Bool
hasRecursion g = hasRec g (S.singleton "main") (lookupRef g "main")

hasRec :: Grammar -> S.Set String -> Pattern -> Bool
hasRec _ _ Empty = False
hasRec _ _ ZAny = False
hasRec _ _ Node{} = False
hasRec g set (Or l r) = hasRec g set l || hasRec g set r
hasRec g set (And l r) = hasRec g set l || hasRec g set r
hasRec g set (Not p) = hasRec g set p
hasRec g set (Concat l r) = hasRec g set l || (nullable g l && hasRec g set r)
hasRec g set (Interleave l r) = hasRec g set l || hasRec g set r
hasRec _ _ (ZeroOrMore _) = False
hasRec g set (Optional p) = hasRec g set p
hasRec g set (Contains p) = hasRec g set p
hasRec g set (Reference refName) = S.member refName set || hasRec g (S.insert refName set) (lookupRef g refName)
