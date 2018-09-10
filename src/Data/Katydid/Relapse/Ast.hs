-- |
-- This module describes the Relapse's abstract syntax tree.
--
-- It also contains some simple functions for the map of references that a Relapse grammar consists of.
--
-- Finally it also contains some very simple pattern functions.
module Data.Katydid.Relapse.Ast (
    Pattern(..)
    , Grammar, emptyRef, union, newRef, reverseLookupRef, lookupRef, hasRecursion, listRefs
    , nullable
) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.Extra ((||^), (&&^))

import Data.Katydid.Relapse.Expr

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
nullable :: Grammar -> Pattern -> Either String Bool
nullable _ Empty = Right True
nullable _ ZAny = Right True
nullable _ Node{} = Right False
nullable g (Or l r) = nullable g l ||^ nullable g r
nullable g (And l r) = nullable g l &&^ nullable g r
nullable g (Not p) = not <$> nullable g p
nullable g (Concat l r) = nullable g l &&^ nullable g r
nullable g (Interleave l r) = nullable g l &&^ nullable g r
nullable _ (ZeroOrMore _) = Right True
nullable _ (Optional _) = Right True
nullable g (Contains p) = nullable g p
nullable g (Reference refName) = lookupRef g refName >>= nullable g

-- |
-- Refs is a map from reference name to pattern and describes a relapse grammar.
newtype Grammar = Grammar (M.Map String Pattern)
    deriving (Show, Eq)

-- |
-- lookupRef looks up a pattern in the reference map, given a reference name.
lookupRef :: Grammar -> String -> Either String Pattern
lookupRef (Grammar m) refName = case M.lookup refName m of
    Nothing -> Left $ "missing reference: " ++ refName
    (Just p) -> Right p

-- |
-- listRefs returns the list of reference names.
listRefs :: Grammar -> [String]
listRefs (Grammar m) = M.keys m

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
hasRecursion :: Grammar -> Either String Bool
hasRecursion g = do {
    mainPat <- lookupRef g "main";
    hasRec g (S.singleton "main") mainPat 
}

hasRec :: Grammar -> S.Set String -> Pattern -> Either String Bool
hasRec _ _ Empty = Right False
hasRec _ _ ZAny = Right False
hasRec _ _ Node{} = Right False
hasRec g set (Or l r) = hasRec g set l ||^ hasRec g set r
hasRec g set (And l r) = hasRec g set l ||^ hasRec g set r
hasRec g set (Not p) = hasRec g set p
hasRec g set (Concat l r) = hasRec g set l ||^ (nullable g l &&^ hasRec g set r)
hasRec g set (Interleave l r) = hasRec g set l ||^ hasRec g set r
hasRec g set (ZeroOrMore p) = hasRec g set p
hasRec g set (Optional p) = hasRec g set p
hasRec g set (Contains p) = hasRec g set p
hasRec g set (Reference refName) = if S.member refName set
    then Right True
    else do {
        pat <- lookupRef g refName;
        hasRec g (S.insert refName set) pat;
    }
