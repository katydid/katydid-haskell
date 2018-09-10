-- |
-- This module describes the smart constructors for Relapse patterns.
module Data.Katydid.Relapse.Smart (
    Pattern(..)
    , Grammar
    , lookupRef
    , compile
    , emptyPat, zanyPat, nodePat
    , orPat, andPat, notPat 
    , concatPat, interleavePat
    , zeroOrMorePat, optionalPat
    , containsPat, refPat
    , emptySet
    , unescapable
    , nullable
    , lookupMain
) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (sort, sortBy, intercalate)
import Control.Monad (when)

import qualified Data.Katydid.Relapse.Expr as Expr
import Data.Katydid.Relapse.Exprs.Logic (orExpr, andExpr)
import qualified Data.Katydid.Relapse.Ast as Ast

-- | compile complies an ast into a smart grammar.
compile :: Ast.Grammar -> Either String Grammar
compile g = do {
    Ast.lookupRef g "main"; -- making sure that the main reference exists.
    hasRec <- Ast.hasRecursion g;
    when hasRec $ Left "recursion without interleaved treenode not supported";
    refs <- M.fromList <$> mapM (\name -> do {
        p <- Ast.lookupRef g name;
        return (name, p)
    }) (Ast.listRefs g);
    nullRefs <- mapM (Ast.nullable g) refs;
    Grammar <$> mapM (smart nullRefs) refs
}

smart :: M.Map String Bool -> Ast.Pattern -> Either String Pattern
smart _ Ast.Empty = return emptyPat
smart nulls (Ast.Node e p) = nodePat e <$> smart nulls p
smart nulls (Ast.Concat a b) = concatPat <$> smart nulls a <*> smart nulls b
smart nulls (Ast.Or a b) = orPat <$> smart nulls a <*> smart nulls b
smart nulls (Ast.And a b) = andPat <$> smart nulls a <*> smart nulls b
smart nulls (Ast.ZeroOrMore p) = zeroOrMorePat <$> smart nulls p
smart nulls (Ast.Reference name) = refPat nulls name
smart nulls (Ast.Not p) = notPat <$> smart nulls p
smart _ Ast.ZAny = return zanyPat
smart nulls (Ast.Contains p) = containsPat <$> smart nulls p
smart nulls (Ast.Optional p) = optionalPat <$> smart nulls p
smart nulls (Ast.Interleave a b) = interleavePat <$> smart nulls a <*> smart nulls b

-- |
-- Pattern recursively describes a Relapse Pattern.
data Pattern = Empty
    | Node {
        expr :: Expr.Expr Bool
        , pat :: Pattern
        , _hash :: Int
    }
    | Concat {
        left :: Pattern
        , right :: Pattern
        , _nullable :: Bool
        , _hash :: Int
    }
    | Or {
        pats :: [Pattern]
        , _nullable :: Bool
        , _hash :: Int
    }
    | And {
        pats :: [Pattern]
        , _nullable :: Bool
        , _hash :: Int
    }
    | ZeroOrMore {
        pat :: Pattern
        , _hash :: Int
    }
    | Reference {
        refName :: ValidRef
        , _nullable :: Bool
        , _hash :: Int
    }
    | Not {
        pat :: Pattern
        , _nullable :: Bool
        , _hash :: Int
    }
    | ZAny
    | Contains {
        pat :: Pattern
        , _nullable :: Bool
        , _hash :: Int
    }
    | Optional {
        pat :: Pattern
        , _hash :: Int
    }
    | Interleave {
        pats :: [Pattern]
        , _nullable :: Bool
        , _hash :: Int
    }
    deriving (Eq, Ord)

instance Show Pattern where
    show = toStr

toStr :: Pattern -> String
toStr Empty = "<empty>"
toStr Node{expr=e, pat=p} = show e ++ ":" ++ show p
toStr Concat{left=l,right=r} = "[" ++ show l ++ "," ++ show r ++ "]"
toStr Or{pats=ps} = "(" ++ intercalate "|" (map show ps) ++ ")"
toStr And{pats=ps} = "(" ++ intercalate "&" (map show ps) ++ ")"
toStr ZeroOrMore{pat=p} = "(" ++ show p ++ ")*"
toStr Reference{refName=(ValidRef n)} = "@"++n
toStr Not{pat=p} = "!(" ++ show p ++ ")"
toStr ZAny = "*"
toStr Contains{pat=p} = "." ++ show p
toStr Optional{pat=p} = "(" ++ show p ++ ")?"
toStr Interleave{pats=ps} = "{" ++ intercalate ";" (map show ps) ++ "}"

-- cmp is an efficient comparison function for patterns.
-- It is very important that cmp is efficient, 
-- because it is a bottleneck for simplification and smart construction of large queries.
cmp :: Pattern -> Pattern -> Ordering
cmp a b = if hashcmp == EQ then compare a b else hashcmp
    where hashcmp = compare (hash a) (hash b)

-- eq is an efficient comparison function for patterns.
-- It is very important that eq is efficient, 
-- because it is a bottleneck for simplification and smart construction of large queries.
eq :: Pattern -> Pattern -> Bool
eq a b = cmp a b == EQ

hash :: Pattern -> Int
hash Empty = 3
hash Node{_hash=h} = h
hash Concat{_hash=h} = h
hash Or{_hash=h} = h
hash And{_hash=h} = h
hash ZeroOrMore{_hash=h} = h
hash Reference{_hash=h} = h
hash Not{_hash=h} = h
hash ZAny = 5
hash Contains{_hash=h} = h
hash Optional{_hash=h} = h
hash Interleave{_hash=h} = h

-- | nullable returns whether the pattern matches the empty string.
nullable :: Pattern -> Bool
nullable Empty = True
nullable Node{} = False
nullable Concat{_nullable=n} = n
nullable Or{_nullable=n} = n
nullable And{_nullable=n} = n
nullable ZeroOrMore{} = True
nullable Reference{_nullable=n} = n
nullable Not{_nullable=n} = n
nullable ZAny = True
nullable Contains{_nullable=n} = n
nullable Optional{} = True
nullable Interleave{_nullable=n} = n

-- | emptyPat is the smart constructor for the empty pattern.
emptyPat :: Pattern
emptyPat = Empty

-- | zanyPat is the smart constructor for the zany pattern.
zanyPat :: Pattern
zanyPat = ZAny

-- | notPat is the smart constructor for the not pattern.
notPat :: Pattern -> Pattern
notPat Not {pat=p} = p
notPat p = Not {
    pat = p
    , _nullable = not $ nullable p
    , _hash = 31 * 7 + hash p
}

-- | emptySet is the smart constructor for the !(*) pattern.
emptySet :: Pattern
emptySet = notPat zanyPat

-- | nodePat is the smart constructor for the node pattern.
nodePat :: Expr.Expr Bool -> Pattern -> Pattern
nodePat e p =
    case Expr.evalConst e of
    (Just False) -> emptySet
    _ -> Node {
        expr = e
        , pat = p
        , _hash = 31 * (11 + 31 * Expr._hash (Expr.desc e)) + hash p
    }

isLeaf :: Pattern -> Bool
isLeaf Node{pat=Empty} = True
isLeaf _ = False

-- | concatPat is the smart constructor for the concat pattern.
concatPat :: Pattern -> Pattern -> Pattern
concatPat notZAny@Not{pat=ZAny} _ = notZAny
concatPat _ notZAny@Not{pat=ZAny} = notZAny
concatPat Empty b = b
concatPat a Empty = a
concatPat Concat{left=a1, right=a2} b = concatPat a1 (concatPat a2 b)
concatPat ZAny Concat{left=b1, right=ZAny} = containsPat b1
concatPat a b = Concat {
    left = a
    , right = b 
    , _nullable = nullable a && nullable b
    , _hash = 31 * (13 + 31 * hash a) + hash b
}

-- | containsPat is the smart constructor for the contains pattern.
containsPat :: Pattern -> Pattern
containsPat Empty = ZAny
containsPat p@ZAny = p
containsPat p@Not{pat=ZAny} = p
containsPat p = Contains {
    pat = p
    , _nullable = nullable p
    , _hash = 31 * 17 + hash p
}

-- | optionalPat is the smart constructor for the optional pattern.
optionalPat :: Pattern -> Pattern
optionalPat p@Empty = p
optionalPat p@Optional{} = p
optionalPat p = Optional {
    pat = p
    , _hash = 31 * 19 + hash p
}

-- | zeroOrMorePat is the smart constructor for the zeroOrMore pattern.
zeroOrMorePat :: Pattern -> Pattern
zeroOrMorePat p@ZeroOrMore{} = p
zeroOrMorePat p = ZeroOrMore {
    pat = p
    , _hash = 31 * 23 + hash p
}

-- | refPat is the smart constructor for the reference pattern.
refPat :: M.Map String Bool -> String -> Either String Pattern
refPat nullRefs name = 
    case M.lookup name nullRefs of
        Nothing -> Left $ "no reference named: " ++ name
        (Just n) -> Right Reference {
            refName = ValidRef name
            , _hash = 31 * 29 + Expr.hashString name
            , _nullable = n
        }

-- | orPat is the smart constructor for the or pattern.
orPat :: Pattern -> Pattern -> Pattern
orPat a b = orPat' $ S.fromList (getOrs a ++ getOrs b)

getOrs :: Pattern -> [Pattern]
getOrs Or{pats=ps} = ps
getOrs p = [p]

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
    \ps -> let psList = sort $ S.toList ps
    in  Or {
            pats = psList
            , _nullable = any nullable psList
            , _hash = Expr.hashList (31*33) $ map hash psList
        }

-- | andPat is the smart constructor for the and pattern.
andPat :: Pattern -> Pattern -> Pattern
andPat a b = andPat' $ S.fromList (getAnds a ++ getAnds b)

getAnds :: Pattern -> [Pattern]
getAnds And{pats=ps} = ps
getAnds p = [p]

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
    \ps -> let psList = sort $ S.toList ps 
    in And {
        pats = psList
        , _nullable = all nullable psList
        , _hash = Expr.hashList (31*37) $ map hash psList
    }

-- | returnIfSingleton returns the pattern from the set if the set is of size one, otherwise it applies the function to the set.
returnIfSingleton :: S.Set Pattern -> (S.Set Pattern -> Pattern) -> Pattern
returnIfSingleton s1 f =
    if S.size s1 == 1 then head $ S.toList s1 else f s1

mergeLeaves :: (Expr.Expr Bool -> Expr.Expr Bool -> Expr.Expr Bool) -> S.Set Pattern -> S.Set Pattern
mergeLeaves merger = merge $ \a b -> case (a,b) of
    (Node{expr=ea,pat=Empty},Node{expr=eb,pat=Empty}) -> [nodePat (merger ea eb) emptyPat]
    _ -> [a,b]

mergeNodesWithEqualNames :: (Pattern -> Pattern -> Pattern) -> S.Set Pattern -> S.Set Pattern
mergeNodesWithEqualNames merger = merge $ \a b -> case (a,b) of
    (Node{expr=ea,pat=pa},Node{expr=eb,pat=pb}) -> 
        if ea == eb then [nodePat ea (merger pa pb)] else [a,b]
    _ -> [a,b]

merge :: (Pattern -> Pattern -> [Pattern]) -> S.Set Pattern -> S.Set Pattern
merge merger ps = let list = sortBy leavesThenNamesAndThenContains (S.toList ps)
    in S.fromList $ foldl (\(a:merged) b -> merger a b ++ merged) [head list] (tail list)

leavesThenNamesAndThenContains :: Pattern -> Pattern -> Ordering
leavesThenNamesAndThenContains a@Node{} b@Node{} = leavesFirst a b
leavesThenNamesAndThenContains Node{} _ = LT
leavesThenNamesAndThenContains _ Node{} = GT
leavesThenNamesAndThenContains a b = containsThird a b

leavesFirst :: Pattern -> Pattern -> Ordering
leavesFirst a b
    | isLeaf a && isLeaf b = compare a b
    | isLeaf a = LT
    | isLeaf b = GT
    | otherwise = namesSecond a b

namesSecond :: Pattern -> Pattern -> Ordering
namesSecond a@Node{expr=ea} b@Node{expr=eb} = let fcomp = compare ea eb
    in if fcomp == EQ 
        then compare a b
        else fcomp

containsThird :: Pattern -> Pattern -> Ordering
containsThird a@Contains{} b@Contains{} = compare a b
containsThird Contains{} _ = LT
containsThird _ Contains{} = GT
containsThird a b = compare a b

-- | interleavePat is the smart constructor for the interleave pattern.
interleavePat :: Pattern -> Pattern -> Pattern
interleavePat a b = interleavePat' (getInterleaves a ++ getInterleaves b)

getInterleaves :: Pattern -> [Pattern]
getInterleaves Interleave{pats=ps} = ps
getInterleaves p = [p]

interleavePat' :: [Pattern] -> Pattern
interleavePat' ps
    | emptySet `elem` ps = emptySet
    | all (eq Empty) ps = emptyPat
    | otherwise = delete Empty ps `returnIfOnlyOne`
        \ps -> (if any (eq ZAny) ps
            then zanyPat : delete ZAny ps
            else ps) `returnIfOnlyOne`
        \ps -> let psList = sort ps
        in Interleave {
            pats = psList
            , _nullable = all nullable psList
            , _hash = Expr.hashList (31*41) $ map hash psList
        }

-- | returnIfOnlyOne returns the pattern from the list if the list is of size one, otherwise it applies the function to the list.
returnIfOnlyOne :: [Pattern] -> ([Pattern] -> Pattern) -> Pattern
returnIfOnlyOne xs f = if length xs == 1 then head xs else f xs

delete :: Pattern -> [Pattern] -> [Pattern]
delete removeItem = filter (not . (\p -> p == removeItem))

-- |
-- unescapable is used for short circuiting.
-- A part of the tree can be skipped if all patterns are unescapable.
unescapable :: Pattern -> Bool
unescapable ZAny = True
unescapable Not{pat=ZAny} = True
unescapable _ = False

-- |
-- Grammar is a map from reference name to pattern and describes a relapse grammar.
newtype Grammar = Grammar Refs
    deriving (Show, Eq)

-- |
-- Refs is a map from reference name to pattern, excluding the main reference, which makes a relapse grammar.
type Refs = M.Map String Pattern

newtype ValidRef = ValidRef String
    deriving (Eq, Ord, Show)

-- |
-- lookupRef looks up a pattern in the reference map, given a reference name.
lookupRef :: Grammar -> ValidRef -> Pattern
lookupRef (Grammar refs) (ValidRef name) = 
    case M.lookup name refs of
        Nothing -> error $ "valid reference not found: " ++ name
        (Just p) -> p

-- | lookupMain retrieves the main pattern from the grammar.
lookupMain :: Grammar -> Pattern
lookupMain g = lookupRef g (ValidRef "main")
