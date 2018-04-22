 -- |
-- This module describes the patterns supported by Relapse.
--
-- It also contains some simple functions for the map of references that a Relapse grammar consists of.
--
-- Finally it also contains some very simple pattern functions.
module Patterns (
    Pattern(..), PatternType(..)
    , emptyPat, zanyPat, nodePat
    , orPat, andPat, notPat 
    , concatPat, interleavePat
    , zeroOrMorePat, optionalPat
    , containsPat, refPat
    , emptySet
    , Refs, emptyRef, union, newRef, reverseLookupRef, lookupRef, hasRecursion
    , unescapable
) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (sort, sortBy)
import Data.Maybe (mapMaybe, fromJust)
import Control.Monad.State (State, runState, lift, state)
import Control.Monad.Except (Except, throwError, runExcept, mapExcept)

import Expr
import Exprs.Logic (orExpr, andExpr)

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

emptySet :: Pattern
emptySet = notPat zanyPat

nodePat :: Expr Bool -> Pattern -> Pattern
nodePat e p = 
    case evalConst e of
    (Just False) -> emptySet
    _ -> Pattern {
        typ = Node
        , func = Just e
        , patterns = [p]
        , ref = Nothing
        , hash = 31 * (11 + 31 * _hash (desc e)) + hash p
        , nullable = False
    }

isLeaf :: Pattern -> Bool
isLeaf p = typ p == Node && (typ $ head $ patterns p) == Empty

isNotZAny :: Pattern -> Bool
isNotZAny p = typ p == Not && typ (head (patterns p)) == ZAny

concatPat :: Pattern -> Pattern -> Pattern
concatPat a b
    | isNotZAny a = emptySet
    | isNotZAny b = emptySet
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

refPat :: Grammar -> String -> Either String Grammar
refPat g n = Grammar (Pattern {
        typ = Reference
        , func = Nothing
        , patterns = []
        , ref = Just n
        , hash = 31 * 29 + hashString n
        , nullable = nullable $ lookupRef g n
    }, emptyRef)

orPat :: Pattern -> Pattern -> Pattern
orPat a b = orPat' $ S.fromList (getOrs a ++ getOrs b)

getOrs :: Pattern -> [Pattern]
getOrs p = if typ p == Or
    then patterns p
    else [p]

orPat' :: S.Set Pattern -> Pattern
orPat' ps = ps `returnIfSingleton`
    \ps -> if S.member zanyPat ps
        then zanyPat
        else S.delete emptySet ps `returnIfSingleton`
    \ps -> (if all nullable ps
        then S.delete emptyPat ps
        else ps) `returnIfSingleton`
    \ps -> mergeLeaves orExpr ps `returnIfSingleton`
    \ps -> mergeNodesWithEqualNames orPat ps `returnIfSingleton`
    \ps -> let psList = sort $ S.toList ps in
    Pattern {
        typ = Or
        , func = Nothing
        , patterns = psList
        , ref = Nothing
        , hash = hashList (31*33) $ map hash psList
        , nullable = any nullable psList
    }

andPat :: Pattern -> Pattern -> Pattern
andPat a b = andPat' $ S.fromList (getAnds a ++ getAnds b)

getAnds :: Pattern -> [Pattern]
getAnds p = if typ p == And
    then patterns p
    else [p]

andPat' :: S.Set Pattern -> Pattern
andPat' ps = ps `returnIfSingleton`
    \ps -> if S.member emptySet ps
        then emptySet
        else S.delete zanyPat ps `returnIfSingleton`
    \ps -> if S.member emptyPat ps
        then if all nullable ps
            then emptyPat
            else emptySet 
        else ps `returnIfSingleton`
    \ps -> mergeLeaves andExpr ps `returnIfSingleton`
    \ps -> mergeNodesWithEqualNames andPat ps `returnIfSingleton`
    \ps -> let psList = sort $ S.toList ps in
    Pattern {
        typ = And
        , func = Nothing
        , patterns = psList
        , ref = Nothing
        , hash = hashList (31*37) $ map hash psList
        , nullable = all nullable psList
    }

-- | returnIfSingleton returns the pattern from the set if the set is of size one, otherwise it applies the function to the set.
returnIfSingleton :: S.Set Pattern -> (S.Set Pattern -> Pattern) -> Pattern
returnIfSingleton s1 f =
    if S.size s1 == 1 then head $ S.toList s1 else f s1

mergeLeaves :: (Expr Bool -> Expr Bool -> Expr Bool) -> S.Set Pattern -> S.Set Pattern
mergeLeaves merger = merge $ \a b -> 
    if isLeaf a && isLeaf b 
        then [nodePat (merger (fromJust $ func a) (fromJust $ func b)) emptyPat]
        else [a,b]

mergeNodesWithEqualNames :: (Pattern -> Pattern -> Pattern) -> S.Set Pattern -> S.Set Pattern
mergeNodesWithEqualNames merger = merge $ \a b ->
    if typ a == Node && typ b == Node && func a == func b
        then [nodePat (fromJust $ func a) (merger (head $ patterns a) (head $ patterns b))]
        else [a,b]

merge :: (Pattern -> Pattern -> [Pattern]) -> S.Set Pattern -> S.Set Pattern
merge merger ps = let list = sortBy leavesThenNamesAndThenContains (S.toList ps)
    in S.fromList $ foldl (\(a:merged) b -> merger a b ++ merged) [head list] (tail list)

leavesThenNamesAndThenContains :: Pattern -> Pattern -> Ordering
leavesThenNamesAndThenContains a b
    | typ a == Node && typ b /= Node = LT
    | typ b == Node && typ a /= Node = GT
    | typ a == Node && typ b == Node = leavesFirst a b
    | otherwise = containsThird a b

leavesFirst :: Pattern -> Pattern -> Ordering
leavesFirst a b
    | isLeaf a && isLeaf b = compare a b
    | isLeaf a = LT
    | isLeaf b = GT
    | otherwise = namesSecond a b

namesSecond :: Pattern -> Pattern -> Ordering
namesSecond a b = compare (func a) (func b) <> compare a b

containsThird :: Pattern -> Pattern -> Ordering
containsThird a b
    | typ a == Contains && typ b == Contains = compare a b
    | typ a == Contains = LT
    | typ b == Contains = GT
    | otherwise = compare a b

interleavePat :: Pattern -> Pattern -> Pattern
interleavePat a b = interleavePat' (getInterleaves a ++ getInterleaves b)

getInterleaves :: Pattern -> [Pattern]
getInterleaves p = if typ p == Interleave
    then patterns p
    else [p]

interleavePat' :: [Pattern] -> Pattern
interleavePat' ps
    | emptySet `elem` ps = emptySet
    | all (\p -> typ p == Empty) ps = emptyPat
    | otherwise = delete Empty ps `returnIfOnlyOne`
        \ps -> (if any (\p -> typ p == ZAny) ps
            then zanyPat : delete ZAny ps
            else ps) `returnIfOnlyOne`
        \ps -> let psList = sort ps
        in Pattern {
            typ = Interleave
            , func = Nothing
            , patterns = psList
            , ref = Nothing
            , hash = hashList (31*41) $ map hash psList
            , nullable = all nullable psList
        }

-- | returnIfOnlyOne returns the pattern from the list if the list is of size one, otherwise it applies the function to the list.
returnIfOnlyOne :: [Pattern] -> ([Pattern] -> Pattern) -> Pattern
returnIfOnlyOne xs f = if length xs == 1 then head xs else f xs

delete :: PatternType -> [Pattern] -> [Pattern]
delete pt = filter (not . (\p -> typ p == pt))

-- |
-- unescapable is used for short circuiting.
-- A part of the tree can be skipped if all patterns are unescapable.
unescapable :: Pattern -> Bool
unescapable p = typ p == ZAny || isNotZAny p

-- |
-- Grammar is a map from reference name to pattern and describes a relapse grammar.
newtype Grammar = Grammar (Pattern, Refs)
    deriving (Show, Eq)

-- |
-- Refs is a map from reference name to pattern, excluding the main reference, which makes a relapse grammar.
type Refs = M.Map String Pattern

-- |
-- lookupRef looks up a pattern in the reference map, given a reference name.
lookupRef :: Grammar -> String -> Except String Pattern
lookupRef (Grammar (main, m)) name = if name == "main" 
    then main 
    else case M.lookup m name of
        Nothing -> throwError $ "no reference: " ++ name
        (Just p) -> return p

-- |
-- reverseLookupRef returns the reference name for a given pattern.
reverseLookupRef :: Pattern -> Grammar -> Maybe String
reverseLookupRef p (Grammar (main, m)) = if p == main
    then Just "main"
    else case M.keys $ M.filter (== p) m of
        []      -> Nothing
        (k:_)  -> Just k

-- |
-- newRef returns a new reference map given a single pattern and its reference name.
newRef :: String -> Pattern -> Refs
newRef = M.singleton

-- |
-- emptyRef returns an empty reference map.
emptyRef :: Refs
emptyRef = M.empty

-- |
-- union returns the union of two reference maps.
union :: Refs -> Refs -> Refs
union = M.union

-- |
-- hasRecursion returns whether an relapse grammar has any recursion, starting from the "main" reference.
hasRecursion :: Grammar -> Bool
hasRecursion g@(Grammar (main, _)) = hasRec g (S.singleton "main") main

hasRec :: Grammar -> S.Set String -> Pattern -> Except String Bool
hasRec g set p = let 
    hRec = hasRec g set
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
            in if S.member name set 
                then return True
                else (hasRec g (S.insert name set)) <$> (lookupRef g name)
