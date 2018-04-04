-- |
-- This module describes the patterns supported by Relapse.
--
-- It also contains some simple functions for the map of references that a Relapse grammar consists of.
--
-- Finally it also contains some very simple pattern functions.
module Patterns (
    Pattern
    , emptyPat, zanyPat, nodePat
    , orPat, andPat, notPat 
    , concatPat, interleavePat
    , zeroOrMorePat, optionalPat
    , containsPat, refPat
    , Refs, emptyRef, union, newRef, reverseLookupRef, lookupRef, hasRecursion
    , nullable, unescapable
) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.State (State, runState, lift, state)

import Expr

data PatternType = Empty
    | Node
    | Concat
    | Or
    | And
    | ZeroOrMore
    | Reference
    | Not
    | ZAny
    | Contains
    | Optional
    | Interleave
    deriving (Show, Ord, Eq)

-- |
-- Pattern recursively describes a Relapse Pattern.
data Pattern = Pattern {
    typ :: PatternType
    , func :: Maybe (Expr Bool)
    , patterns :: [Pattern]
    , ref :: Maybe String
    , hash :: Int
    , nullable :: Bool 
}

instance Show Pattern where
    show p = show (typ p) ++ "{" ++ show (patterns p) ++ "}"

instance Ord Pattern where
    compare = cmp

instance Eq Pattern where
    (==) a b = cmp a b == EQ

(<>) :: Ordering -> Ordering -> Ordering
(<>) EQ c = c
(<>) c _ = c

-- cmp is an efficient comparison function for patterns.
-- It is very important that cmp is efficient, 
-- because it is a bottleneck for simplification and smart construction of large queries.
cmp :: Pattern -> Pattern -> Ordering
cmp a b = compare (hash a) (hash b) <>
    compare (typ a) (typ b) <>
    compare (func a) (func b) <>
    foldl (<>) EQ (zipWith cmp (patterns a) (patterns b)) <>
    compare (ref a) (ref b)

emptyPat :: Pattern
emptyPat = Pattern {
    typ = Empty
    , func = Nothing
    , patterns = []
    , ref = Nothing
    , hash = 3
    , nullable = True
}

zanyPat :: Pattern
zanyPat = Pattern {
    typ = ZAny
    , func = Nothing
    , patterns = []
    , ref = Nothing
    , hash = 5
    , nullable = True
}

notPat :: Pattern -> Pattern
notPat p
    | typ p == Not = head $ patterns p
    | otherwise = Pattern {
        typ = Not
        , func = Nothing
        , patterns = [p]
        , ref = Nothing
        , hash = 31 * 7 + hash p
        , nullable = not $ nullable p
    }

nodePat :: Expr Bool -> Pattern -> Pattern
nodePat e p = 
    case evalConst e of
    (Just False) -> notPat zanyPat
    _ -> Pattern {
        typ = Node
        , func = Just e
        , patterns = [p]
        , ref = Nothing
        , hash = 31 * (11 + 31 * _hash (desc e)) + hash p
        , nullable = False
    }

isNotZAny :: Pattern -> Bool
isNotZAny p = typ p == Not && typ (head (patterns p)) == ZAny

concatPat :: Pattern -> Pattern -> Pattern
concatPat a b
    | isNotZAny a = notPat zanyPat
    | isNotZAny b = notPat zanyPat
    | typ a == Empty = b
    | typ b == Empty = a
    | typ a == Concat = concatPat (head $ patterns a) $ concatPat (patterns a !! 1) b
    | typ a == ZAny && typ b == Concat && typ (patterns b !! 1) == ZAny = containsPat (head $ patterns b)
    | otherwise = Pattern {
        typ = Concat
        , func = Nothing
        , patterns = [a, b]
        , ref = Nothing
        , hash = 31 * (13 + 31 * hash a) + hash b
        , nullable = nullable a && nullable b
    }

containsPat :: Pattern -> Pattern
containsPat p
    | typ p == Empty = zanyPat
    | typ p == ZAny = zanyPat
    | isNotZAny p = p
    | otherwise = Pattern {
        typ = Contains
        , func = Nothing
        , patterns = [p]
        , ref = Nothing
        , hash = 31 * 17 + hash p
        , nullable = nullable p
    }

optionalPat :: Pattern -> Pattern
optionalPat p
    | typ p == Empty = emptyPat
    | typ p == Optional = p
    | otherwise = Pattern {
        typ = Optional
        , func = Nothing
        , patterns = [p]
        , ref = Nothing
        , hash = 31 * 19 + hash p
        , nullable = True
    }

zeroOrMorePat :: Pattern -> Pattern
zeroOrMorePat p
    | typ p == ZeroOrMore = p
    | otherwise = Pattern {
        typ = ZeroOrMore
        , func = Nothing
        , patterns = [p]
        , ref = Nothing
        , hash = 31 * 23 + hash p
        , nullable = True
    }

refPat :: String -> Pattern
refPat n = Pattern {
        typ = Reference
        , func = Nothing
        , patterns = []
        , ref = Just n
        , hash = 31 * 29 + hashString n
        , nullable = True
    }

-- |
-- unescapable is used for short circuiting.
-- A part of the tree can be skipped if all patterns are unescapable.
unescapable :: Pattern -> Bool
unescapable p = typ p == ZAny || isNotZAny p

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
hasRec refs set p = let 
    hRec = hasRec refs set
    p0 = head $ patterns p
    in case typ p of
        Empty -> False
        ZAny -> False
        Node -> False
        Or -> any hRec (patterns p)
        And -> any hRec (patterns p)
        Interleave -> any hRec (patterns p)
        Not -> hRec p0
        Concat -> hRec p0 || (nullable p0 && hRec (patterns p !! 1))
        ZeroOrMore -> hRec p0
        Optional -> hRec p0
        Contains -> hRec p0
        Reference -> 
            let (Just name) = ref p
            in S.member name set || 
                hasRec refs (S.insert name set) (lookupRef refs name)
