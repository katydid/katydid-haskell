module Expr (
    evalExpr
    , Bytes, Uint
    , Desc(..), mkDesc
    , AnyExpr(..), AnyFunc(..)
    , Expr(..), params, name, hasVar
    , hashWithName
    , evalConst, isConst
    , assertArgs0, assertArgs1, assertArgs2
    , mkBoolExpr, mkIntExpr, mkStringExpr, mkDoubleExpr, mkBytesExpr, mkUintExpr
    , assertBool, assertInt, assertString, assertDouble, assertBytes, assertUint
    , boolExpr, intExpr, stringExpr, doubleExpr, bytesExpr, uintExpr
    , trimBool, trimInt, trimString, trimDouble, trimBytes, trimUint
    , mkBoolsExpr, mkIntsExpr, mkStringsExpr, mkDoublesExpr, mkListOfBytesExpr, mkUintsExpr
    , assertBools, assertInts, assertStrings, assertDoubles, assertListOfBytes, assertUints
    , boolsExpr, intsExpr, stringsExpr, doublesExpr, listOfBytesExpr, uintsExpr
) where

import Data.Char (ord)
import Control.Monad.Except (Except, runExcept, throwError)
import Data.List (intercalate)

import qualified Parsers

evalExpr :: AnyExpr -> Parsers.Label -> Except String Bool
evalExpr (AnyExpr _ (BoolFunc evalBool)) v = evalBool v
evalExpr (AnyExpr desc _) _ = throwError $ "not a bool expr, but " ++ show desc

assertArgs0 :: String -> [AnyExpr] -> Except String ()
assertArgs0 name [] = return ()
assertArgs0 name es = throwError $ name ++ ": expected zero arguments, but got " ++ show (length es) ++ ": " ++ show es

assertArgs1 :: String -> [AnyExpr] -> Except String AnyExpr
assertArgs1 name (e1:[]) = return e1
assertArgs1 name es = throwError $ name ++ ": expected one argument, but got " ++ show (length es) ++ ": " ++ show es

assertArgs2 :: String -> [AnyExpr] -> Except String (AnyExpr, AnyExpr)
assertArgs2 name (e1:e2:[]) = return (e1, e2)
assertArgs2 name es = throwError $ name ++ ": expected two arguments, but got " ++ show (length es) ++ ": " ++ show es

data Desc = Desc {
    _name :: String
    , _toStr :: String
    , _hash :: Int
    , _params :: [Desc]
    , _hasVar :: Bool
}

mkDesc :: String -> [Desc] -> Desc
mkDesc n ps = Desc {
    _name = n
    , _toStr = n ++ "(" ++ (intercalate "," $ map show ps) ++ ")"
    , _hash = hashWithName n ps
    , _params = ps
    , _hasVar = any _hasVar ps
}

instance Show Desc where
    show = _toStr

instance Ord Desc where
    compare = cmp

instance Eq Desc where
    (==) a b = (cmp a b) == EQ

data AnyExpr = AnyExpr {
    _desc :: Desc
    , _eval :: AnyFunc
}

type Func a = (Parsers.Label -> Except String a)

instance Show AnyExpr where
    show a = show (_desc a)

instance Eq AnyExpr where
    (==) a b = (_desc a) == (_desc b)

instance Ord AnyExpr where
    compare a b = cmp (_desc a) (_desc b)

data AnyFunc = BoolFunc (Func Bool)
    | IntFunc (Func Int)
    | StringFunc (Func String)
    | DoubleFunc (Func Double)
    | UintFunc (Func Uint)
    | BytesFunc (Func Bytes)
    | BoolsFunc (Func [Bool])
    | IntsFunc (Func [Int])
    | StringsFunc (Func [String])
    | DoublesFunc (Func [Double])
    | UintsFunc (Func [Uint])
    | ListOfBytesFunc (Func [Bytes])

data Expr a = Expr {
    desc :: Desc
    , eval :: Func a
}

instance Show (Expr a) where
    show e = show (desc e)

instance Eq (Expr a) where
    (==) x y = (desc x) == (desc y)

instance Ord (Expr a) where
    compare x y = cmp (desc x) (desc y)

params :: Expr a -> [Desc]
params = _params . desc

name :: Expr a -> String
name = _name . desc

hasVar :: Expr a -> Bool
hasVar = _hasVar . desc

mkBoolExpr :: Expr Bool -> AnyExpr
mkBoolExpr (Expr desc eval) = AnyExpr desc (BoolFunc eval)

assertBool :: AnyExpr -> Except String (Expr Bool)
assertBool (AnyExpr desc (BoolFunc eval)) = return $ Expr desc eval
assertBool (AnyExpr desc _) = throwError $ "expected <" ++ show desc ++ "> to be of type bool"

mkIntExpr :: Expr Int -> AnyExpr
mkIntExpr (Expr desc eval) = AnyExpr desc (IntFunc eval)

assertInt :: AnyExpr -> Except String (Expr Int)
assertInt (AnyExpr desc (IntFunc eval)) = return $ Expr desc eval
assertInt (AnyExpr desc _) = throwError $ "expected <" ++ show desc ++ "> to be of type int"

mkDoubleExpr :: Expr Double -> AnyExpr
mkDoubleExpr (Expr desc eval) = AnyExpr desc (DoubleFunc eval)

assertDouble :: AnyExpr -> Except String (Expr Double)
assertDouble (AnyExpr desc (DoubleFunc eval)) = return $ Expr desc eval
assertDouble (AnyExpr desc _) = throwError $ "expected <" ++ show desc ++ "> to be of type double"

mkStringExpr :: Expr String -> AnyExpr
mkStringExpr (Expr desc eval) = AnyExpr desc (StringFunc eval)

assertString :: AnyExpr -> Except String (Expr String)
assertString (AnyExpr desc (StringFunc eval)) = return $ Expr desc eval
assertString (AnyExpr desc _) = throwError $ "expected <" ++ show desc ++ "> to be of type string"

mkBoolsExpr :: Expr [Bool] -> AnyExpr
mkBoolsExpr (Expr desc eval) = AnyExpr desc (BoolsFunc eval)

assertBools :: AnyExpr -> Except String (Expr [Bool])
assertBools (AnyExpr desc (BoolsFunc eval)) = return $ Expr desc eval
assertBools (AnyExpr desc _) = throwError $ "expected <" ++ show desc ++ "> to be of type bools"

mkIntsExpr :: Expr [Int] -> AnyExpr
mkIntsExpr (Expr desc eval) = AnyExpr desc (IntsFunc eval)

assertInts :: AnyExpr -> Except String (Expr [Int])
assertInts (AnyExpr desc (IntsFunc eval)) = return $ Expr desc eval
assertInts (AnyExpr desc _) = throwError $ "expected <" ++ show desc ++ "> to be of type ints"

mkUintsExpr :: Expr [Uint] -> AnyExpr
mkUintsExpr (Expr desc eval) = AnyExpr desc (UintsFunc eval)

assertUints :: AnyExpr -> Except String (Expr [Uint])
assertUints (AnyExpr desc (UintsFunc eval)) = return $ Expr desc eval
assertUints (AnyExpr desc _) = throwError $ "expected <" ++ show desc ++ "> to be of type uints"

mkDoublesExpr :: Expr [Double] -> AnyExpr
mkDoublesExpr (Expr desc eval) = AnyExpr desc (DoublesFunc eval)

assertDoubles :: AnyExpr -> Except String (Expr [Double])
assertDoubles (AnyExpr desc (DoublesFunc eval)) = return $ Expr desc eval
assertDoubles (AnyExpr desc _) = throwError $ "expected <" ++ show desc ++ "> to be of type doubles"

mkStringsExpr :: Expr [String] -> AnyExpr
mkStringsExpr (Expr desc eval) = AnyExpr desc (StringsFunc eval)

assertStrings :: AnyExpr -> Except String (Expr [String])
assertStrings (AnyExpr desc (StringsFunc eval)) = return $ Expr desc eval
assertStrings (AnyExpr desc _) = throwError $ "expected <" ++ show desc ++ "> to be of type strings"

mkListOfBytesExpr :: Expr [Bytes] -> AnyExpr
mkListOfBytesExpr (Expr desc eval) = AnyExpr desc (ListOfBytesFunc eval)

assertListOfBytes :: AnyExpr -> Except String (Expr [Bytes])
assertListOfBytes (AnyExpr desc (ListOfBytesFunc eval)) = return $ Expr desc eval
assertListOfBytes (AnyExpr desc _) = throwError $ "expected <" ++ show desc ++ "> to be of type bytes"

type Uint = Int

mkUintExpr :: Expr Uint -> AnyExpr
mkUintExpr (Expr desc eval) = AnyExpr desc (UintFunc eval)

assertUint :: AnyExpr -> Except String (Expr Uint)
assertUint (AnyExpr desc (UintFunc eval)) = return $ Expr desc eval
assertUint (AnyExpr desc _) = throwError $ "expected <" ++ show desc ++ "> to be of type uint"

type Bytes = String

mkBytesExpr :: Expr Bytes -> AnyExpr
mkBytesExpr (Expr desc eval) = AnyExpr desc (BytesFunc eval)

assertBytes :: AnyExpr -> Except String (Expr Bytes)
assertBytes (AnyExpr desc (BytesFunc eval)) = return $ Expr desc eval
assertBytes (AnyExpr desc _) = throwError $ "expected <" ++ show desc ++ "> to be of type bytes"

(<>) :: Ordering -> Ordering -> Ordering
(<>) EQ c = c
(<>) c _ = c

-- cmp is an efficient comparison function for expressions.
-- It is very important that cmp is efficient, 
-- because it is a bottleneck for simplification and smart construction of large queries.
cmp :: Desc -> Desc -> Ordering
cmp a b = compare (_hash a) (_hash b) <>
    compare (_name a) (_name b) <>
    compare (length (_params a)) (length (_params b)) <>
    foldl (<>) EQ (zipWith cmp (_params a) (_params b)) <>
    compare (_toStr a) (_toStr b)

hashWithName :: String -> [Desc] -> Int
hashWithName s ds = hashList (31*17 + hashString s) (map _hash ds)

hashString :: String -> Int
hashString s = hashList 0 (map ord s)

-- hashList folds a list of hashes into one, given a seed and the list.
hashList :: Int -> [Int] -> Int
hashList = foldl (\acc h -> 31*acc + h)

noLabel :: Parsers.Label
noLabel = Parsers.String "not a variable"

evalConst :: Expr a -> Maybe a
evalConst e = if hasVar e
    then Nothing
    else case runExcept $ eval e noLabel of
        (Left _) -> Nothing
        (Right v) -> Just v

isConst :: Desc -> Bool
isConst d = length (_params d) /= 0 && case _name d of
    "bool" -> True
    "int" -> True
    "uint" -> True
    "double" -> True
    "string" -> True
    "[]byte" -> True
    _ -> False

boolExpr :: Bool -> Expr Bool 
boolExpr b = Expr {
    desc = Desc {
        _name = "bool"
        , _toStr = if b then "true" else "false"
        , _hash = if b then 3 else 5
        , _params = []
        , _hasVar = False
    }
    , eval = const $ return b
}

intExpr :: Int -> Expr Int
intExpr i = Expr {
    desc = Desc {
        _name = "int"
        , _toStr = show i
        , _hash = i
        , _params = []
        , _hasVar = False
    }
    , eval = const $ return i
}

doubleExpr :: Double -> Expr Double
doubleExpr d = Expr {
    desc = Desc {
        _name = "double"
        , _toStr = show d
        , _hash = truncate d
        , _params = []
        , _hasVar = False
    }
    , eval = const $ return d
}

uintExpr :: Uint -> Expr Uint
uintExpr i = Expr {
    desc = Desc {
        _name = "uint"
        , _toStr = show i
        , _hash = i
        , _params = []
        , _hasVar = False
    }
    , eval = const $ return i
}

stringExpr :: String -> Expr String
stringExpr s = Expr {
    desc = Desc {
        _name = "string"
        , _toStr = show s
        , _hash = hashString s
        , _params = []
        , _hasVar = False
    }
    , eval = const $ return s
}

bytesExpr :: Bytes -> Expr Bytes
bytesExpr b = Expr {
    desc = Desc {
        _name = "bytes"
        , _toStr = "[]byte{" ++ b ++ "}"
        , _hash = hashString b
        , _params = []
        , _hasVar = False
    }
    , eval = const $ return b
}

trimBool :: Expr Bool -> Expr Bool
trimBool e = if hasVar e 
    then e
    else case runExcept $ eval e noLabel of
        (Left _) -> e
        (Right v) -> boolExpr v

trimInt :: Expr Int -> Expr Int
trimInt e = if hasVar e 
    then e
    else case runExcept $ eval e noLabel of
        (Left _) -> e
        (Right v) -> intExpr v

trimUint :: Expr Uint -> Expr Uint
trimUint e = if hasVar e 
    then e
    else case runExcept $ eval e noLabel of
        (Left _) -> e
        (Right v) -> uintExpr v

trimString :: Expr String -> Expr String
trimString e = if hasVar e 
    then e
    else case runExcept $ eval e noLabel of
        (Left _) -> e
        (Right v) -> stringExpr v

trimDouble :: Expr Double -> Expr Double
trimDouble e = if hasVar e 
    then e
    else case runExcept $ eval e noLabel of
        (Left _) -> e
        (Right v) -> doubleExpr v

trimBytes :: Expr Bytes -> Expr Bytes
trimBytes e = if hasVar e 
    then e
    else case runExcept $ eval e noLabel of
        (Left _) -> e
        (Right v) -> bytesExpr v

boolsExpr :: [Expr Bool] -> Expr [Bool]
boolsExpr = seqExprs "[]bool" 

intsExpr :: [Expr Int] -> Expr [Int]
intsExpr = seqExprs "[]int"

stringsExpr :: [Expr String] -> Expr [String]
stringsExpr = seqExprs "[]string"

doublesExpr :: [Expr Double] -> Expr [Double]
doublesExpr = seqExprs "[]double"

listOfBytesExpr :: [Expr Bytes] -> Expr [Bytes]
listOfBytesExpr = seqExprs "[][]byte"

uintsExpr :: [Expr Uint] -> Expr [Uint]
uintsExpr = seqExprs "[]uint"

seqExprs :: String -> [Expr a] -> Expr [a]
seqExprs n es = Expr {
    desc = mkDesc n (map desc es)
    , eval = \v -> mapM (`eval` v) es
}