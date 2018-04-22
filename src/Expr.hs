-- |
-- This module contains all the functions you need to implement a Relapse expression.

module Expr (
    Desc(..), mkDesc
    , AnyExpr(..), AnyFunc(..)
    , Expr(..), Func, params, name, hasVar
    , hashWithName, hashString, hashList
    , evalConst, isConst
    , assertArgs1, assertArgs2
    , mkBoolExpr, mkIntExpr, mkStringExpr, mkDoubleExpr, mkBytesExpr, mkUintExpr
    , assertBool, assertInt, assertString, assertDouble, assertBytes, assertUint
    , boolExpr, intExpr, stringExpr, doubleExpr, bytesExpr, uintExpr
    , trimBool, trimInt, trimString, trimDouble, trimBytes, trimUint
    , mkBoolsExpr, mkIntsExpr, mkStringsExpr, mkDoublesExpr, mkListOfBytesExpr, mkUintsExpr
    , assertBools, assertInts, assertStrings, assertDoubles, assertListOfBytes, assertUints
    , boolsExpr, intsExpr, stringsExpr, doublesExpr, listOfBytesExpr, uintsExpr
) where

import Data.Char (ord)
import Data.List (intercalate)
import Data.Text (Text, unpack, pack)
import Data.ByteString (ByteString)

import qualified Parsers

-- |
-- assertArgs1 asserts that the list of arguments is only one argument and 
-- returns the argument or an error message 
-- containing the function name that was passed in as an argument to assertArgs1.
assertArgs1 :: String -> [AnyExpr] -> Either String AnyExpr
assertArgs1 _ [e1] = Right e1
assertArgs1 exprName es = Left $ exprName ++ ": expected one argument, but got " ++ show (length es) ++ ": " ++ show es

-- |
-- assertArgs2 asserts that the list of arguments is only two arguments and 
-- returns the two arguments or an error message 
-- containing the function name that was passed in as an argument to assertArgs2.
assertArgs2 :: String -> [AnyExpr] -> Either String (AnyExpr, AnyExpr)
assertArgs2 _ [e1, e2] = Right (e1, e2)
assertArgs2 exprName es = Left $ exprName ++ ": expected two arguments, but got " ++ show (length es) ++ ": " ++ show es

-- |
-- Desc is the description of a function, 
-- especially built to make comparisons of user defined expressions possible.
data Desc = Desc {
    _name :: String
    , _toStr :: String
    , _hash :: Int
    , _params :: [Desc]
    , _hasVar :: Bool
}

-- |
-- mkDesc makes a description from a function name and a list of the argument's descriptions.
mkDesc :: String -> [Desc] -> Desc
mkDesc n ps = Desc {
    _name = n
    , _toStr = n ++ "(" ++ intercalate "," (map show ps) ++ ")"
    , _hash = hashWithName n ps
    , _params = ps
    , _hasVar = any _hasVar ps
}

instance Show Desc where
    show = _toStr

instance Ord Desc where
    compare = cmp

instance Eq Desc where
    (==) a b = cmp a b == EQ

-- |
-- AnyExpr is used by the Relapse parser to represent an Expression that can return any type of value, 
-- where any is a predefined list of possible types represented by AnyFunc.
data AnyExpr = AnyExpr {
    _desc :: Desc
    , _eval :: AnyFunc
}

-- |
-- Func represents the evaluation function part of a user defined expression.
-- This function takes a label from a tree parser and returns a value or an error string.
type Func a = (Parsers.Label -> Either String a)

instance Show AnyExpr where
    show a = show (_desc a)

instance Eq AnyExpr where
    (==) a b = _desc a == _desc b

instance Ord AnyExpr where
    compare a b = cmp (_desc a) (_desc b)

-- |
-- AnyFunc is used by the Relapse parser and represents the list all supported types of functions.
data AnyFunc = BoolFunc (Func Bool)
    | IntFunc (Func Int)
    | StringFunc (Func Text)
    | DoubleFunc (Func Double)
    | UintFunc (Func Word)
    | BytesFunc (Func ByteString)
    | BoolsFunc (Func [Bool])
    | IntsFunc (Func [Int])
    | StringsFunc (Func [Text])
    | DoublesFunc (Func [Double])
    | UintsFunc (Func [Word])
    | ListOfBytesFunc (Func [ByteString])

-- |
-- Expr represents a user defined expression, 
-- which consists of a description for comparisons and an evaluation function.
data Expr a = Expr {
    desc :: Desc
    , eval :: Func a
}

instance Show (Expr a) where
    show e = show (desc e)

instance Eq (Expr a) where
    (==) x y = desc x == desc y

instance Ord (Expr a) where
    compare x y = cmp (desc x) (desc y)

-- |
-- params returns the descriptions of the parameters of the user defined expression.
params :: Expr a -> [Desc]
params = _params . desc

-- |
-- name returns the name of the user defined expression.
name :: Expr a -> String
name = _name . desc

-- |
-- hasVar returns whether the expression or any of its children contains a variable expression.
hasVar :: Expr a -> Bool
hasVar = _hasVar . desc

-- |
-- mkBoolExpr generalises a bool expression to any expression.
mkBoolExpr :: Expr Bool -> AnyExpr
mkBoolExpr (Expr desc eval) = AnyExpr desc (BoolFunc eval)

-- |
-- assertBool asserts that any expression is actually a bool expression.
assertBool :: AnyExpr -> Either String (Expr Bool)
assertBool (AnyExpr desc (BoolFunc eval)) = Right $ Expr desc eval
assertBool (AnyExpr desc _) = Left $ "expected <" ++ show desc ++ "> to be of type bool"

-- |
-- mkIntExpr generalises an int expression to any expression.
mkIntExpr :: Expr Int -> AnyExpr
mkIntExpr (Expr desc eval) = AnyExpr desc (IntFunc eval)

-- |
-- assertInt asserts that any expression is actually an int expression.
assertInt :: AnyExpr -> Either String (Expr Int)
assertInt (AnyExpr desc (IntFunc eval)) = Right $ Expr desc eval
assertInt (AnyExpr desc _) = Left $ "expected <" ++ show desc ++ "> to be of type int"

-- |
-- mkDoubleExpr generalises a double expression to any expression.
mkDoubleExpr :: Expr Double -> AnyExpr
mkDoubleExpr (Expr desc eval) = AnyExpr desc (DoubleFunc eval)

-- |
-- assertDouble asserts that any expression is actually a double expression.
assertDouble :: AnyExpr -> Either String (Expr Double)
assertDouble (AnyExpr desc (DoubleFunc eval)) = Right $ Expr desc eval
assertDouble (AnyExpr desc _) = Left $ "expected <" ++ show desc ++ "> to be of type double"

-- |
-- mkStringExpr generalises a string expression to any expression.
mkStringExpr :: Expr Text -> AnyExpr
mkStringExpr (Expr desc eval) = AnyExpr desc (StringFunc eval)

-- |
-- assertString asserts that any expression is actually a string expression.
assertString :: AnyExpr -> Either String (Expr Text)
assertString (AnyExpr desc (StringFunc eval)) = Right $ Expr desc eval
assertString (AnyExpr desc _) = Left $ "expected <" ++ show desc ++ "> to be of type string"

-- |
-- mkUintExpr generalises a uint expression to any expression.
mkUintExpr :: Expr Word -> AnyExpr
mkUintExpr (Expr desc eval) = AnyExpr desc (UintFunc eval)

-- |
-- assertUint asserts that any expression is actually a uint expression.
assertUint :: AnyExpr -> Either String (Expr Word)
assertUint (AnyExpr desc (UintFunc eval)) = Right $ Expr desc eval
assertUint (AnyExpr desc _) = Left $ "expected <" ++ show desc ++ "> to be of type uint"

-- |
-- mkBytesExpr generalises a bytes expression to any expression.
mkBytesExpr :: Expr ByteString -> AnyExpr
mkBytesExpr (Expr desc eval) = AnyExpr desc (BytesFunc eval)

-- |
-- assertBytes asserts that any expression is actually a bytes expression.
assertBytes :: AnyExpr -> Either String (Expr ByteString)
assertBytes (AnyExpr desc (BytesFunc eval)) = Right $ Expr desc eval
assertBytes (AnyExpr desc _) = Left $ "expected <" ++ show desc ++ "> to be of type bytes"

-- |
-- mkBoolsExpr generalises a list of bools expression to any expression.
mkBoolsExpr :: Expr [Bool] -> AnyExpr
mkBoolsExpr (Expr desc eval) = AnyExpr desc (BoolsFunc eval)

-- |
-- assertBools asserts that any expression is actually a list of bools expression.
assertBools :: AnyExpr -> Either String (Expr [Bool])
assertBools (AnyExpr desc (BoolsFunc eval)) = Right $ Expr desc eval
assertBools (AnyExpr desc _) = Left $ "expected <" ++ show desc ++ "> to be of type bools"

-- |
-- mkIntsExpr generalises a list of ints expression to any expression.
mkIntsExpr :: Expr [Int] -> AnyExpr
mkIntsExpr (Expr desc eval) = AnyExpr desc (IntsFunc eval)

-- |
-- assertInts asserts that any expression is actually a list of ints expression.
assertInts :: AnyExpr -> Either String (Expr [Int])
assertInts (AnyExpr desc (IntsFunc eval)) = Right $ Expr desc eval
assertInts (AnyExpr desc _) = Left $ "expected <" ++ show desc ++ "> to be of type ints"

-- |
-- mkUintsExpr generalises a list of uints expression to any expression.
mkUintsExpr :: Expr [Word] -> AnyExpr
mkUintsExpr (Expr desc eval) = AnyExpr desc (UintsFunc eval)

-- |
-- assertUints asserts that any expression is actually a list of uints expression.
assertUints :: AnyExpr -> Either String (Expr [Word])
assertUints (AnyExpr desc (UintsFunc eval)) = Right $ Expr desc eval
assertUints (AnyExpr desc _) = Left $ "expected <" ++ show desc ++ "> to be of type uints"

-- |
-- mkDoublesExpr generalises a list of doubles expression to any expression.
mkDoublesExpr :: Expr [Double] -> AnyExpr
mkDoublesExpr (Expr desc eval) = AnyExpr desc (DoublesFunc eval)

-- |
-- assertDoubles asserts that any expression is actually a list of doubles expression.
assertDoubles :: AnyExpr -> Either String (Expr [Double])
assertDoubles (AnyExpr desc (DoublesFunc eval)) = Right $ Expr desc eval
assertDoubles (AnyExpr desc _) = Left $ "expected <" ++ show desc ++ "> to be of type doubles"

-- |
-- mkStringsExpr generalises a list of strings expression to any expression.
mkStringsExpr :: Expr [Text] -> AnyExpr
mkStringsExpr (Expr desc eval) = AnyExpr desc (StringsFunc eval)

-- |
-- assertStrings asserts that any expression is actually a list of strings expression.
assertStrings :: AnyExpr -> Either String (Expr [Text])
assertStrings (AnyExpr desc (StringsFunc eval)) = Right $ Expr desc eval
assertStrings (AnyExpr desc _) = Left $ "expected <" ++ show desc ++ "> to be of type strings"

-- |
-- mkListOfBytesExpr generalises a list of bytes expression to any expression.
mkListOfBytesExpr :: Expr [ByteString] -> AnyExpr
mkListOfBytesExpr (Expr desc eval) = AnyExpr desc (ListOfBytesFunc eval)

-- |
-- assertListOfBytes asserts that any expression is actually a list of bytes expression.
assertListOfBytes :: AnyExpr -> Either String (Expr [ByteString])
assertListOfBytes (AnyExpr desc (ListOfBytesFunc eval)) = Right $ Expr desc eval
assertListOfBytes (AnyExpr desc _) = Left $ "expected <" ++ show desc ++ "> to be of type bytes"

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

-- |
-- hashWithName calculates a hash of the function name and its parameters.
hashWithName :: String -> [Desc] -> Int
hashWithName s ds = hashList (31*17 + hashString s) (map _hash ds)

-- |
-- hashString calcuates a hash of a string.
hashString :: String -> Int
hashString s = hashList 0 (map ord s)

-- |
-- hashList folds a list of hashes into one, given a seed and the list.
hashList :: Int -> [Int] -> Int
hashList = foldl (\acc h -> 31*acc + h)

noLabel :: Parsers.Label
noLabel = Parsers.String (pack "not a label, trying constant evaluation")

-- |
-- evalConst tries to evaluate a constant expression and 
-- either returns the resulting constant value or nothing.
evalConst :: Expr a -> Maybe a
evalConst e = if hasVar e
    then Nothing
    else case eval e noLabel of
        (Left _) -> Nothing
        (Right v) -> Just v

-- |
-- isConst returns whether the input description is one of the six possible constant values.
isConst :: Desc -> Bool
isConst d = not (null (_params d)) && case _name d of
    "bool" -> True
    "int" -> True
    "uint" -> True
    "double" -> True
    "string" -> True
    "[]byte" -> True
    _ -> False

-- |
-- boolExpr creates a constant bool expression from a input value.
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

-- |
-- intExpr creates a constant int expression from a input value.
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

-- |
-- doubleExpr creates a constant double expression from a input value.
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

-- |
-- uintExpr creates a constant uint expression from a input value.
uintExpr :: Word -> Expr Word
uintExpr i = Expr {
    desc = Desc {
        _name = "uint"
        , _toStr = show i
        , _hash = hashString (show i)
        , _params = []
        , _hasVar = False
    }
    , eval = const $ return i
}

-- |
-- stringExpr creates a constant string expression from a input value.
stringExpr :: Text -> Expr Text
stringExpr s = Expr {
    desc = Desc {
        _name = "string"
        , _toStr = show s
        , _hash = hashString (unpack s)
        , _params = []
        , _hasVar = False
    }
    , eval = const $ return s
}

-- |
-- bytesExpr creates a constant bytes expression from a input value.
bytesExpr :: ByteString -> Expr ByteString
bytesExpr b = Expr {
    desc = Desc {
        _name = "bytes"
        , _toStr = "[]byte{" ++ show b ++ "}"
        , _hash = hashString (show b)
        , _params = []
        , _hasVar = False
    }
    , eval = const $ return b
}

-- |
-- trimBool tries to reduce an expression to a single constant expression,
-- if it does not contain a variable.
trimBool :: Expr Bool -> Expr Bool
trimBool e = if hasVar e 
    then e
    else case eval e noLabel of
        (Left _) -> e
        (Right v) -> boolExpr v

-- |
-- trimInt tries to reduce an expression to a single constant expression,
-- if it does not contain a variable.
trimInt :: Expr Int -> Expr Int
trimInt e = if hasVar e 
    then e
    else case eval e noLabel of
        (Left _) -> e
        (Right v) -> intExpr v

-- |
-- trimUint tries to reduce an expression to a single constant expression,
-- if it does not contain a variable.
trimUint :: Expr Word -> Expr Word
trimUint e = if hasVar e 
    then e
    else case eval e noLabel of
        (Left _) -> e
        (Right v) -> uintExpr v

-- |
-- trimString tries to reduce an expression to a single constant expression,
-- if it does not contain a variable.
trimString :: Expr Text -> Expr Text
trimString e = if hasVar e 
    then e
    else case eval e noLabel of
        (Left _) -> e
        (Right v) -> stringExpr v

-- |
-- trimDouble tries to reduce an expression to a single constant expression,
-- if it does not contain a variable.
trimDouble :: Expr Double -> Expr Double
trimDouble e = if hasVar e 
    then e
    else case eval e noLabel of
        (Left _) -> e
        (Right v) -> doubleExpr v

-- |
-- trimBytes tries to reduce an expression to a single constant expression,
-- if it does not contain a variable.
trimBytes :: Expr ByteString -> Expr ByteString
trimBytes e = if hasVar e 
    then e
    else case eval e noLabel of
        (Left _) -> e
        (Right v) -> bytesExpr v

-- |
-- boolsExpr sequences a list of expressions that each return a bool, 
-- to a single expression that returns a list of bools.
boolsExpr :: [Expr Bool] -> Expr [Bool]
boolsExpr = seqExprs "[]bool" 

-- |
-- intsExpr sequences a list of expressions that each return an int, 
-- to a single expression that returns a list of ints.
intsExpr :: [Expr Int] -> Expr [Int]
intsExpr = seqExprs "[]int"

-- |
-- stringsExpr sequences a list of expressions that each return a string, 
-- to a single expression that returns a list of strings.
stringsExpr :: [Expr Text] -> Expr [Text]
stringsExpr = seqExprs "[]string"

-- |
-- doublesExpr sequences a list of expressions that each return a double, 
-- to a single expression that returns a list of doubles.
doublesExpr :: [Expr Double] -> Expr [Double]
doublesExpr = seqExprs "[]double"

-- |
-- listOfBytesExpr sequences a list of expressions that each return bytes, 
-- to a single expression that returns a list of bytes.
listOfBytesExpr :: [Expr ByteString] -> Expr [ByteString]
listOfBytesExpr = seqExprs "[][]byte"

-- |
-- uintsExpr sequences a list of expressions that each return a uint, 
-- to a single expression that returns a list of uints.
uintsExpr :: [Expr Word] -> Expr [Word]
uintsExpr = seqExprs "[]uint"

seqExprs :: String -> [Expr a] -> Expr [a]
seqExprs n es = Expr {
    desc = mkDesc n (map desc es)
    , eval = \v -> mapM (`eval` v) es
}